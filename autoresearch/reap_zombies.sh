#!/bin/zsh
# reap_zombies.sh -- kill ONLY leaked/orphaned script-mode WolframKernel procs.
#
# A wolframscript launcher spawns a `WolframKernel -runfirst ... Script` child.
# If the launcher is SIGKILLed or crashes, that kernel is reparented to PID 1
# (orphaned) and can spin at 100% CPU forever -> the machine eventually dies.
#
# SAFE BY CONSTRUCTION. We only kill a kernel if BOTH:
#   (a) it is script-mode  (command contains  $EvaluationEnvironment="Script")
#       -> never touches the interactive WolframNB / front-end kernels, and
#   (b) it is orphaned      (ppid == 1)
#       -> never touches a kernel whose wolframscript launcher is still alive,
#          so other sessions' RUNNING jobs are untouched.
#
# Usage:
#   reap_zombies.sh          # report + reap, print what it did
#   reap_zombies.sh --dry    # report only, kill nothing

dry=0
[[ "$1" == "--dry" ]] && dry=1

# pids of script-mode kernels that are orphaned (ppid 1)
victims=()
while read -r pid ppid rest; do
  [[ "$ppid" == "1" ]] && victims+=("$pid")
done < <(ps -eo pid,ppid,command | grep 'WolframKernel' | grep 'EvaluationEnvironment="Script"' | grep -v grep)

if (( ${#victims[@]} == 0 )); then
  echo "[reap] no orphaned script kernels"
  exit 0
fi

echo "[reap] orphaned script kernels: ${victims[*]}"
if (( dry )); then
  echo "[reap] --dry: nothing killed"
  exit 0
fi

# TERM first, then KILL stragglers
kill -TERM "${victims[@]}" 2>/dev/null
sleep 3
still=()
for p in "${victims[@]}"; do
  kill -0 "$p" 2>/dev/null && still+=("$p")
done
if (( ${#still[@]} > 0 )); then
  kill -KILL "${still[@]}" 2>/dev/null
  echo "[reap] SIGKILLed stragglers: ${still[*]}"
fi
echo "[reap] reaped ${#victims[@]} orphaned kernel(s)"
