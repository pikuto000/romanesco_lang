import sys
import os

# Add the current directory to sys.path to allow importing the romanesco package
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from romanesco.__main__ import main

if __name__ == "__main__":
    main()
