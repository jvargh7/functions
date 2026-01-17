"""
Convert Jupyter notebook to a text file containing code cells.

This function reads a .ipynb file and exports all code cells to a .txt file,
with cell delimiters for later reconstruction.
"""

import json
from pathlib import Path


def jupyter_to_text(notebook_path, output_path=None):
    """
    Convert a Jupyter notebook to a text file with code cells.
    
    Parameters
    ----------
    notebook_path : str or Path
        Path to the input .ipynb file
    output_path : str or Path, optional
        Path to the output .txt file. If None, uses the same name as the
        notebook with .txt extension
    
    Returns
    -------
    str
        Path to the created text file
    
    Examples
    --------
    >>> jupyter_to_text("analysis.ipynb")
    'analysis.txt'
    
    >>> jupyter_to_text("analysis.ipynb", "code_backup.txt")
    'code_backup.txt'
    """
    notebook_path = Path(notebook_path)
    
    if not notebook_path.exists():
        raise FileNotFoundError(f"Notebook file not found: {notebook_path}")
    
    if output_path is None:
        output_path = notebook_path.with_suffix('.txt')
    else:
        output_path = Path(output_path)
    
    # Read the notebook
    with open(notebook_path, 'r', encoding='utf-8') as f:
        notebook = json.load(f)
    
    # Extract code cells
    cells = notebook.get('cells', [])
    
    # Write to text file
    with open(output_path, 'w', encoding='utf-8') as f:
        for i, cell in enumerate(cells):
            cell_type = cell.get('cell_type', 'code')
            source = cell.get('source', [])
            
            # Convert source to string
            if isinstance(source, list):
                source_text = ''.join(source)
            else:
                source_text = source
            
            # Write cell delimiter and type
            f.write(f"### CELL {i} ### TYPE: {cell_type} ###\n")
            f.write(source_text)
            
            # Ensure there's a newline at the end
            if source_text and not source_text.endswith('\n'):
                f.write('\n')
            
            # Add separator between cells
            f.write("### END CELL ###\n\n")
    
    return str(output_path)


if __name__ == "__main__":
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python jupyter_concatenator.py <notebook_path> [output_path]")
        sys.exit(1)
    
    notebook_path = sys.argv[1]
    output_path = sys.argv[2] if len(sys.argv) > 2 else None
    
    result = jupyter_to_text(notebook_path, output_path)
    print(f"Created text file: {result}")
