import sys
import matplotlib.pyplot as plt
import numpy as np

plt.figure(figsize=(6, 4))

# Dictionary to store statistics
stats = {}

# First pass to get all data and determine common bin edges
all_data = np.concatenate([np.loadtxt(f) * 10**6 for f in sys.argv[1:]])
min_val = np.min(all_data)
max_val = np.percentile(all_data, 99.5)  # Use 99.5 percentile to exclude extreme outliers

# Print information about outliers
print("Outlier information (values beyond 99.5 percentile):")
outlier_threshold = max_val
print(f"Outlier threshold: {outlier_threshold:.2f}μs")

for filename in sys.argv[1:]:
    data = np.loadtxt(filename) * 10**6
    outliers = data[data > outlier_threshold]
    
    if len(outliers) > 0:
        print(f"\nOutliers in {filename}:")
        print(f"  Count: {len(outliers)}")
        print(f"  Values: {', '.join([f'{x:.2f}μs' for x in sorted(outliers)])}")
        print(f"  Min: {np.min(outliers):.2f}μs, Max: {np.max(outliers):.2f}μs")
    else:
        print(f"\nNo outliers found in {filename}")

# Create common bin edges with fixed width
bin_width = (max_val - min_val) / 10  # Calculate bin width based on range and desired number of bins
bins = np.arange(min_val, max_val + bin_width, bin_width)  # Create explicit bin edges

# Load and plot data
for i, filename in enumerate(sys.argv[1:]):
    data = np.loadtxt(filename)
    
    # Convert to microseconds
    data_micro = data * 10**6
    
    # Filter data to only include values up to 99.5 percentile
    filtered_data = data_micro[data_micro <= outlier_threshold]
    
    # Calculate statistics on filtered data (excluding outliers)
    mean = np.mean(filtered_data)
    median = np.median(filtered_data)
    std_dev = np.std(filtered_data)
    
    stats[filename] = {
        'mean': mean,
        'median': median,
        'std_dev': std_dev
    }
    
    # Determine label based on filename
    if "fixed" in filename.lower():
        label = f"Fixed Input (std={std_dev:.2f}μs)"
    elif "random" in filename.lower():
        label = f"Different Input (std={std_dev:.2f}μs)"
    else:
        label = f"{filename} (std={std_dev:.2f}μs)"

    # Plot histogram with shared bins to ensure same width
    plt.hist(data_micro, bins=bins, alpha=0.7, label=label)

plt.xlim(min_val, max_val)
plt.xlabel('Microseconds')
plt.ylabel('Number of Occurrences')
plt.grid(True, alpha=0.3)
plt.legend()

# # Add a statistical summary as text
# plt.figtext(0.5, 0.01, "\n".join([
#     f"{filename}: mean={stats[filename]['mean']:.2f}μs, median={stats[filename]['median']:.2f}μs, std={stats[filename]['std_dev']:.2f}μs"
#     for filename in sys.argv[1:]
# ]), ha='center', fontsize=10, bbox={"facecolor":"lightgray", "alpha":0.5, "pad":5})

# plt.tight_layout(rect=[0, 0.05, 1, 0.95])  # Adjust layout to make room for the text
plt.savefig('timing_leaks_test.png', dpi=300)
plt.close()
