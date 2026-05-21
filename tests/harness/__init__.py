# tests/harness — KGPC test harness package.
# Import order matters: env patches subprocess at import time, so it must load
# before any other module that might call subprocess.
from . import env  # noqa: F401
