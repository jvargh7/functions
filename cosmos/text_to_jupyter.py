"""
Convert a text file back to a Jupyter notebook.

This function reads a .txt file created by jupyter_concatenator.py and
reconstructs the original Jupyter notebook structure.
"""

import json
import re
from pathlib import Path


def text_to_jupyter(text_path, output_path=None, kernel_name="python3"):
    """
    Convert a text file back to a Jupyter notebook.
    
    Parameters
    ----------
    text_path : str or Path
        Path to the input .txt file created by jupyter_to_text
    output_path : str or Path, optional
        Path to the output .ipynb file. If None, uses the same name as the
        text file with .ipynb extension
    kernel_name : str, optional
        Name of the kernel to use (default: "python3")
    
    Returns
    -------
    str
        Path to the created notebook file
    
    Examples
    --------
    >>> text_to_jupyter("analysis.txt")
    'analysis.ipynb'
    
    >>> text_to_jupyter("code_backup.txt", "restored.ipynb")
    'restored.ipynb'
    """
    text_path = Path(text_path)
    
    if not text_path.exists():
        raise FileNotFoundError(f"Text file not found: {text_path}")
    
    if output_path is None:
        output_path = text_path.with_suffix('.ipynb')
    else:
        output_path = Path(output_path)
    
    # Read the text file
    with open(text_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Parse cells
    cells = []
    cell_pattern = r'### CELL \d+ ### TYPE: (\w+) ###\n(.*?)### END CELL ###'
    matches = re.finditer(cell_pattern, content, re.DOTALL)
    
    for match in matches:
        cell_type = match.group(1)
        source_text = match.group(2)
        
        # Remove trailing newline if present
        if source_text.endswith('\n'):
            source_text = source_text[:-1]
        
        # Convert source to list of lines
        source_lines = []
        if source_text:
            lines = source_text.split('\n')
            for i, line in enumerate(lines):
                if i < len(lines) - 1:
                    source_lines.append(line + '\n')
                else:
                    source_lines.append(line)
        
        # Create cell structure
        cell = {
            "cell_type": cell_type,
            "metadata": {},
            "source": source_lines
        }
        
        # Add execution_count for code cells
        if cell_type == "code":
            cell["execution_count"] = None
            cell["outputs"] = []
        
        cells.append(cell)
    
    # Create notebook structure
    notebook = {
        "cells": cells,
        "metadata": {
            "kernelspec": {
                "display_name": "Python 3",
                "language": "python",
                "name": kernel_name
            },
            "language_info": {
                "name": "python",
                "version": "3.8.0"
            }
        },
        "nbformat": 4,
        "nbformat_minor": 4
    }
    
    # Write the notebook
    with open(output_path, 'w', encoding='utf-8') as f:
        json.dump(notebook, f, indent=2)
    
    return str(output_path)


if __name__ == "__main__":
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python jupyter_splitter.py <text_path> [output_path]")
        sys.exit(1)
    
    text_path = sys.argv[1]
    output_path = sys.argv[2] if len(sys.argv) > 2 else None
    
    result = text_to_jupyter(text_path, output_path)
    print(f"Created notebook file: {result}")
