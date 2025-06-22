import os
import shutil

def file_transfers(src_dir, dst_dir, file_ext):
    """
    Recursively move files of a specific type from src_dir to dst_dir,
    preserving the nested folder structure.

    Args:
        src_dir (str): Source directory to search files in.
        dst_dir (str): Destination directory to move files to.
        file_ext (str): File extension to filter by (e.g., '.csv').
    """
    src_dir = os.path.abspath(src_dir)
    dst_dir = os.path.abspath(dst_dir)

    for root, _, files in os.walk(src_dir):
        for file in files:
            if file.lower().endswith(file_ext.lower()):
                src_file_path = os.path.join(root, file)
                # Compute relative path from src_dir
                rel_path = os.path.relpath(root, src_dir)
                dst_folder = os.path.join(dst_dir, rel_path)
                os.makedirs(dst_folder, exist_ok=True)
                dst_file_path = os.path.join(dst_folder, file)
                shutil.copy2(src_file_path, dst_file_path)

# Example usage:
# move_files_by_type('/path/to/source', '/path/to/destination', '.csv')