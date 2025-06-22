import os
import glob

# Take a folder with Python and R files and concatenate them into a single file.
# Do not consolidate other types of files (git, excel, csv, etc.).
def code_concatenator(folder_path, output_file):
    # Find all .py and .r files in the folder and subfolders (recursive)
    py_files = glob.glob(os.path.join(folder_path, "**", "*.py"), recursive=True)
    r_files = glob.glob(os.path.join(folder_path, "**", "*.r"), recursive=True)
    code_files = py_files + r_files

    with open(output_file, "w", encoding="utf-8") as outfile:
        for file_path in code_files:
            rel_path = os.path.relpath(file_path, folder_path)
            with open(file_path, "r", encoding="utf-8") as infile:
                outfile.write(f"# --- Start of {rel_path} ---\n")
                outfile.write(infile.read())
                outfile.write(f"\n# --- End of {rel_path} ---\n\n")


