#!/usr/bin/env python3
import unittest
import sys
import importlib
import os

# Add the project root to the path to allow importing 'tests.test_runner'
# when the script is run from a subdirectory (like GPC/).
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))


def discover_tests(class_name, module_name):
    """
    Discovers test methods in a given class from a module.
    """
    try:
        module = importlib.import_module(module_name)
        test_class = getattr(module, class_name)
        test_names = [
            method for method in dir(test_class)
            if method.startswith('test_') and callable(getattr(test_class, method))
        ]
        return test_names
    except (ImportError, AttributeError) as e:
        print(f"Error discovering tests in {module_name}.{class_name}: {e}", file=sys.stderr)
        return []

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python discover_tests.py <ClassName> <ModuleName>", file=sys.stderr)
        sys.exit(1)

    class_name_arg = sys.argv[1]
    module_name_arg = sys.argv[2]

    test_names = discover_tests(class_name_arg, module_name_arg)
    for name in test_names:
        print(name)
