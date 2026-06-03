#!/bin/zsh
# supervise.sh -- run the long autoresearch search with zombie-safe teardown.
#
# Problem: a SIGKILLed/crashed wolframscript leaves an orphaned WolframKernel
# spinning at 100% CPU, eventually crashing the machine.
#
# This supervisor:
#   1. Reaps pre-existing orphaned script kernels before starting.
#   2. Launches wolframscript under `timeout` with a HARD wall-clock cap.
#   3. Records its launcher + kernel PIDs to long/pids.txt so they can always be
#      torn down precisely by PID (signal propagation through the wrappers is
#      unreliable; kill-by-PID is not).
#   4. On ANY exit, KILLs the recorded PIDs leaf-to-root, then reaps orphans.
#   5. Periodically reaps orphaned kernels while running.
#
# Usage:  supervise.sh [budgetSeconds]   (default 28800 = 8h)
#         stop:  ./stop.sh   (kills exactly the recorded PIDs)

here="${0:A:h}"
budget="${1:-28800}"
hardcap=$(( budget + 1800 ))
log="$here/long/run.log"
suplog="$here/long/supervisor.log"
pidfile="$here/long/pids.txt"
mkdir -p "$here/long"

ts() { date '+%Y-%m-%d %H:%M:%S'; }
note() { echo "[$(ts)] $*" >> "$suplog"; }

"$here/reap_zombies.sh" >> "$suplog" 2>&1

note "launching long_search.wls budget=$budget hardcap=$hardcap"
timeout -s TERM -k 60 "$hardcap" \
    wolframscript -file "$here/long_search.wls" "$budget" > "$log" 2>&1 &
job=$!
note "launcher(timeout) pid=$job"

# discover the wolframscript + kernel PIDs under this timeout and record them
sleep 8
record_pids() {
  local ws kn
  ws=$(pgrep -P "$job" -f long_search.wls 2>/dev/null)
  kn=""
  [[ -n "$ws" ]] && kn=$(pgrep -P "$ws" -f 'WolframKernel' 2>/dev/null)
  { echo "supervisor=$$"; echo "timeout=$job"; echo "wolframscript=$ws"; echo "kernel=$kn"; } > "$pidfile"
}
record_pids
note "recorded pids -> $pidfile : $(tr '\n' ' ' < "$pidfile")"

cleanup() {
  note "cleanup: tearing down recorded pids"
  # re-read (kernel pid may have appeared after first record)
  record_pids 2>/dev/null
  local kn ws
  kn=$(grep '^kernel=' "$pidfile" 2>/dev/null | cut -d= -f2)
  ws=$(grep '^wolframscript=' "$pidfile" 2>/dev/null | cut -d= -f2)
  [[ -n "$kn" ]] && kill -KILL ${=kn} 2>/dev/null
  [[ -n "$ws" ]] && kill -KILL ${=ws} 2>/dev/null
  kill -KILL "$job" 2>/dev/null
  sleep 2
  "$here/reap_zombies.sh" >> "$suplog" 2>&1
}
trap cleanup EXIT INT TERM

while kill -0 "$job" 2>/dev/null; do
  record_pids 2>/dev/null         # keep pidfile fresh (kernel pid may change)
  "$here/reap_zombies.sh" >> "$suplog" 2>&1
  sleep 300
done

wait "$job"
rc=$?
note "job exited rc=$rc"
trap - EXIT INT TERM
"$here/reap_zombies.sh" >> "$suplog" 2>&1
note "supervisor done"
