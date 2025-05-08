def bin_column_value(value, bins, bin_names):
    """
    Bin a value based on given bins.
    
    Parameters:
    - value: The value to bin
    - bins: A list of tuples defining the bin ranges (lower, upper)
    - bin_names: A list of names for the binned values
    
    Returns:
    - The name of the bin that the value falls into, or 'Invalid' if it doesn't match any bin
    """
    for (lower, upper), bin_name in zip(bins, bin_names):
        if lower <= value <= upper:
            return bin_name
    return "Invalid"  # Return Invalid if no bin matches

def bin_age(row):
    bins = [(18, 25), (26, 35), (36, 50), (51, 70), (71, float('inf'))]
    bin_names = ["18-25", "26-35", "36-50", "51-70", ">70"]
    return bin_column_value(row['Age'], bins, bin_names)

def bin_purchase_amount(row):
    bins = [(20, 40), (41, 70), (71, 100)]
    bin_names = ["Low", "Medium", "High"]
    return bin_column_value(row['Purchase Amount (USD)'], bins, bin_names)

def bin_previous_purchases(row):
    bins = [(0, 5), (6, 10), (11, 15), (16, 20), (21, 25), (26, 30), (31, 35), (36, 40), (41, 45), (46, 50)]
    bin_names = ["0-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50"]
    return bin_column_value(row['Previous Purchases'], bins, bin_names)