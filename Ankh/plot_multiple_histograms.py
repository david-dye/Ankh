import sys
import matplotlib.pyplot as plt
import numpy as np

# Set figure size and style
plt.figure(figsize=(6, 5))  # Adjusted to match reference image proportions
plt.rcParams['axes.linewidth'] = 1.5  # Make plot borders thicker

# Dictionary to store statistics
stats = {}

# First pass to get all data and determine common bin edges
all_data = np.concatenate([np.loadtxt(f) * 10**6 for f in sys.argv[1:]])
min_val = np.min(all_data)
max_val = np.percentile(all_data, 99.5)  # Use 98 percentile to exclude extreme outliers
max_val2 = np.percentile(all_data, 99.5)  # Use 98 percentile to exclude extreme outliers

# Print information about outliers
# print("Outlier information (values beyond 99.5 percentile):")
outlier_threshold = max_val
outlier_threshold2 = max_val2
# print(f"Outlier threshold: {outlier_threshold:.2f}μs")

# for filename in sys.argv[1:]:
#     data = np.loadtxt(filename) * 10**6
#     outliers = data[data > outlier_threshold]
    
#     if len(outliers) > 0:
#         print(f"\nOutliers in {filename}:")
#         print(f"  Count: {len(outliers)}")
#         print(f"  Values: {', '.join([f'{x:.2f}μs' for x in sorted(outliers)])}")
#         print(f"  Min: {np.min(outliers):.2f}μs, Max: {np.max(outliers):.2f}μs")
#     else:
#         print(f"\nNo outliers found in {filename}")

# Create common bin edges with fixed width
bin_width = (max_val - min_val) / 50  # Calculate bin width based on range and desired number of bins
bins = np.arange(min_val, max_val + bin_width, bin_width)  # Create explicit bin edges

# Define colors for files
colors = ['green', 'blue']

# Create plot with gridlines
ax = plt.gca()
ax.grid(True, linestyle='-', alpha=0.7, zorder=0)

# Load and plot data
for i, filename in enumerate(sys.argv[1:]):
    data = np.loadtxt(filename)
    
    # Convert to microseconds
    data_micro = data * 10**6
    
    # Filter data to only include values up to threshold
    filtered_data = data_micro[data_micro <= outlier_threshold]
    filtered_data2 = data_micro[data_micro <= outlier_threshold2]
    
    # Calculate statistics on filtered data (excluding outliers)
    mean = np.mean(filtered_data2)
    median = np.median(filtered_data2)
    std_dev = np.std(filtered_data2)
    
    stats[filename] = {
        'mean': mean,
        'median': median,
        'std_dev': std_dev
    }
    
    # Simple label and color assignment based on file order
    if i == 0:
        color = colors[0]  # First file gets green
    else:
        color = colors[1]  # Second file gets blue

    # if "random" in filename:
    #     label = f"Different Inputs (std={std_dev:.2f}μs)"
    # else:
    #     label = f"Fixed Inputs (std={std_dev:.2f}μs)"

    if "no_opt" in filename:
        label = f"No Optimizations (mean={mean:.2f}μs)"
    elif "opt" in filename:
        label = f"Optimized (mean={mean:.2f}μs)"
    else:
        label = f"No Optimizations (mean={mean:.2f}μs)"
    # Plot histogram with shared bins to ensure same width
    # Add edgecolor='black' to create outlines around bars
    plt.hist(data_micro, bins=bins, alpha=0.5, label=label, color=color, edgecolor='black', linewidth=0.5)

plt.xlim(min_val, max_val)
plt.xlabel('Execution Time (microseconds)', fontsize=12)
plt.ylabel('Frequency', fontsize=12)
plt.tick_params(axis='both', which='major', labelsize=11)
plt.legend(loc='upper right', frameon=True, fontsize=12)

# Create tight layout and add borders
plt.tight_layout()

# Save figure
# filename = "timing_leaks_test.pdf" if "opt" not in sys.argv[1].lower() else "timing_leaks_test_opt.pdf"
filename = "opt_vs_no_opt.pdf"
plt.savefig(filename, dpi=300, bbox_inches='tight')
plt.close()
