
def code_splitter(concatenated_file, output_folder):
    """
    Splits a concatenated code file (created by concatenate_code_files)
    back into individual files in the specified output_folder.
    """
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    with open(concatenated_file, "r", encoding="utf-8") as infile:
        lines = infile.readlines()

    current_file = None
    buffer = []
    rel_path = None

    for line in lines:
        if line.startswith("# --- Start of "):
            rel_path = line[len("# --- Start of "):].strip().rstrip(" ---")
            file_path = os.path.join(output_folder, rel_path)
            os.makedirs(os.path.dirname(file_path), exist_ok=True)
            buffer = []
        elif line.startswith("# --- End of "):
            if rel_path is not None:
                file_path = os.path.join(output_folder, rel_path)
                with open(file_path, "w", encoding="utf-8") as outfile:
                    outfile.writelines(buffer)
                rel_path = None
                buffer = []
        else:
            if rel_path is not None:
                buffer.append(line)