import os
import subprocess
import glob

def run_tests():
    test_files = sorted(glob.glob("tests/*.romanesco"))
    
    print(f"Running {len(test_files)} tests...\n")
    
    for test_file in test_files:
        test_name = os.path.basename(test_file)
        print(f"--- Running {test_name} ---")
        
        try:
            # Run the compiler with eval option
            result = subprocess.run(
                ["python", "Python/main.py", test_file, "eval", "debug"],
                capture_output=True,
                text=True,
                encoding='utf-8'
            )
            
            if result.stdout:
                # Filter out unimportant lines and show the result
                lines = [l for l in result.stdout.splitlines() if l.startswith("Result:") or "solutions" in l]
                for line in lines:
                    print(line)
            
            if result.stderr:
                print(f"Error:\n{result.stderr}")
                
        except Exception as e:
            print(f"Execution failed: {e}")
        
        print()

if __name__ == "__main__":
    run_tests()

