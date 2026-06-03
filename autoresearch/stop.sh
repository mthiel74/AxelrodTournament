#!/bin/zsh
# stop.sh -- stop the supervised autoresearch run by its recorded PIDs.
# Kills exactly the PIDs in long/pids.txt (leaf-to-root) + reaps orphans.
# Never touches the interactive Wolfram front-end or other sessions' jobs.

here="${0:A:h}"
pidfile="$here/long/pids.txt"

if [[ ! -f "$pidfile" ]]; then
  echo "[stop] no pidfile ($pidfile); nothing recorded. Reaping orphans only."
  "$here/reap_zombies.sh"
  exit 0
fi

echo "[stop] recorded:"; cat "$pidfile"
kn=$(grep '^kernel='        "$pidfile" | cut -d= -f2)
ws=$(grep '^wolframscript='  "$pidfile" | cut -d= -f2)
to=$(grep '^timeout='        "$pidfile" | cut -d= -f2)
sv=$(grep '^supervisor='     "$pidfile" | cut -d= -f2)

[[ -n "$kn" ]] && kill -KILL ${=kn} 2>/dev/null
[[ -n "$ws" ]] && kill -KILL ${=ws} 2>/dev/null
[[ -n "$to" ]] && kill -KILL "$to" 2>/dev/null
[[ -n "$sv" ]] && kill -TERM "$sv" 2>/dev/null
sleep 3
"$here/reap_zombies.sh"
echo "[stop] done. remaining long_search procs: $(pgrep -f long_search.wls | wc -l | tr -d ' ')"
