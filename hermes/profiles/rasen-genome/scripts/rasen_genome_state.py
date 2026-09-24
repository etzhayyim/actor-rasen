#!/usr/bin/env python3
"""rasen-genome monitor source. Emits the FLOORS, not the progress.

Hermes suppresses the agent run entirely when this output is byte-identical to the
previous tick, so anything that moves every tick — a row count, a byte total, a
timestamp — would wake the bot continuously during a multi-hour ingest and tell it
nothing. What is emitted here is the small set of states a person would act on.

Credential-free on purpose: this runs unattended, and a monitor that needs a key is a
monitor that goes silent when the key rotates. R2 and kotobase checks belong to the
agent run, which can mint a scoped temp credential per the profile's AGENTS.md.

Every line is "<name>: <value>". A check that could not run says UNMEASURED — never
the same value as a check that ran and found nothing wrong.
"""
import json
import os
import re
import subprocess
import sys

ROOT = os.path.expanduser("~/github/com-junkawasaki")
REPO = os.path.join(ROOT, "orgs/etzhayyim/actor-rasen")
# Durable, not a session scratchpad. A resident bot cannot own state that disappears
# with whoever started it.
LEDGER = os.path.expanduser("~/.gftd/rasen-genome/ledger")


def sh(args, cwd=None):
    try:
        r = subprocess.run(args, cwd=cwd, capture_output=True, text=True, timeout=60)
        return r.stdout.strip() if r.returncode == 0 else None
    except Exception:
        return None


def pin_state():
    """west pin vs the repo's remote main. 'behind' means a fix landed that nobody sees."""
    wy = os.path.join(ROOT, "manifest/west.yml")
    try:
        text = open(wy, encoding="utf-8").read()
    except OSError:
        return "UNMEASURED (west.yml unreadable)"
    m = re.search(r"name: actor-rasen\n(?:.*\n)*?\s+revision: ([0-9a-f]{40})", text)
    if not m:
        return "UNMEASURED (no actor-rasen entry)"
    pin = m.group(1)
    head = sh(["git", "rev-parse", "etzhayyim/main"], cwd=REPO)
    if head is None:
        return "UNMEASURED (remote ref unreadable)"
    return "current" if head == pin else "behind"


def ledger_state():
    """The ledger is machine-local and gitignored; its absence is not a fault."""
    idx = os.path.join(LEDGER, "rasen.genome.shards.edn")
    if not os.path.exists(idx):
        return "absent"
    try:
        open(idx, encoding="utf-8").read()
    except OSError:
        return "UNMEASURED (index unreadable)"
    # Deliberately NOT the shard count. A shard seals every few minutes during an
    # ingest, so emitting it would wake the agent on progress rather than on a floor,
    # which is the failure this monitor exists to avoid.
    return "present"


def checkpoint_status():
    p = os.path.join(LEDGER, "clinvar.checkpoint.edn")
    if not os.path.exists(p):
        return "absent"
    try:
        text = open(p, encoding="utf-8").read()
    except OSError:
        return "UNMEASURED (unreadable)"
    m = re.search(r":run/status\s+(\S+?)\}", text)
    return m.group(1) if m else "UNMEASURED (no status)"


def ingest_running():
    out = sh(["pgrep", "-f", "r2run.clj"])
    return "yes" if out else "no"


def test_signal():
    """Whether the suite is even runnable here. Not whether it passes — that is the
    agent's job and costs minutes."""
    return "present" if os.path.exists(os.path.join(REPO, "run_tests.clj")) else "absent"


def main():
    print("checkout: %s" % ("present" if os.path.isdir(REPO) else "MISSING"))
    print("west-pin: %s" % pin_state())
    print("ledger: %s" % ledger_state())
    print("checkpoint-status: %s" % checkpoint_status())
    print("ingest-process: %s" % ingest_running())
    print("test-entry: %s" % test_signal())


if __name__ == "__main__":
    main()
