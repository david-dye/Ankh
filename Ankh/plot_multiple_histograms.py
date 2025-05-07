import sys
import matplotlib.pyplot as plt
import numpy as np

plt.figure(figsize=(12, 8))

# Dictionary to store statistics
stats = {}

# Load and plot data
for i, filename in enumerate(sys.argv[1:]):
    data = np.loadtxt(filename)
    
    # Convert to microseconds
    data_micro = data * 10**6
    
    # Calculate statistics
    mean = np.mean(data_micro)
    median = np.median(data_micro)
    std_dev = np.std(data_micro)
    
    stats[filename] = {
        'mean': mean,
        'median': median,
        'std_dev': std_dev
    }
    
    # Plot histogram with more bins and focus on relevant range
    plt.hist(data_micro, bins=5000, alpha=0.7, label=f"{filename} (mean={mean:.2f}μs)")

# Find appropriate x-axis limits
all_data = np.concatenate([np.loadtxt(f) * 10**6 for f in sys.argv[1:]])
min_val = np.min(all_data)
max_val = np.percentile(all_data, 99.5)  # Use 99.5 percentile to exclude extreme outliers

plt.xlim(min_val, max_val)
plt.title('Histogram of Execution Times')
plt.xlabel('Microseconds')
plt.ylabel('Frequency')
plt.grid(True, alpha=0.3)
plt.legend()

# Add a statistical summary as text
plt.figtext(0.5, 0.01, "\n".join([
    f"{filename}: mean={stats[filename]['mean']:.2f}μs, median={stats[filename]['median']:.2f}μs, std={stats[filename]['std_dev']:.2f}μs"
    for filename in sys.argv[1:]
]), ha='center', fontsize=10, bbox={"facecolor":"lightgray", "alpha":0.5, "pad":5})

plt.tight_layout(rect=[0, 0.05, 1, 0.95])  # Adjust layout to make room for the text
plt.savefig('multiple_histograms.png', dpi=300)
plt.close()
