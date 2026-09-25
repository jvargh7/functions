


import shutil
from pathlib import Path

from docx2pdf import convert


def move_for_roar(files_list, destination_folder):
    """Convert .docx files to PDF and copy PDFs into destination_folder.

    Returns a list of output PDF paths (in the same order as files_list).
    """
    dest = Path(destination_folder)
    dest.mkdir(parents=True, exist_ok=True)

    output_paths = []
    for f in files_list:
        src = Path(f)
        if not src.exists():
            raise FileNotFoundError(f"File not found: {src}")

        suffix = src.suffix.lower()
        out = dest / f"{src.stem}.pdf"

        if suffix == ".docx":
            convert(str(src), str(out))  # requires MS Word on Windows
        elif suffix == ".pdf":
            if src.resolve() != out.resolve():
                shutil.copy2(src, out)
        else:
            raise ValueError(f"Unsupported file type: {src.name}")

        output_paths.append(out)

    return output_paths

