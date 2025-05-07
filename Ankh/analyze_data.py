import sys
import numpy as np

def analyze_file(filename):
    with open(filename, 'r') as file:
        data = np.loadtxt(file)
    
    average = np.mean(data)
    std_dev = np.std(data)
    
    print(f"Average: {average:.6e}")
    print(f"Standard Deviation: {std_dev:.6e}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python analyze_data.py <filename>")
    else:
        analyze_file(sys.argv[1])

