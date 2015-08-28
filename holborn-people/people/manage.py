#!/nix/store/ss1aq46xwhnqikq5bjs02sb9xrim2bb3-python-2.7.10/bin/python 
import os
import sys

if __name__ == "__main__":
    os.environ.setdefault("DJANGO_SETTINGS_MODULE", "people.settings")

    from django.core.management import execute_from_command_line

    execute_from_command_line(sys.argv)
