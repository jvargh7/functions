from docx2pdf import convert

# custom_ranges = ["1", "2-13", "14-last"]

import os
from pypdf import PdfReader, PdfWriter

def split_docx_to_pdf(docx_path, ranges, output_path, names=None):
    """
    Split a DOCX file into multiple PDFs based on page ranges.

    Parameters
    ----------
    docx_path : str
        Path to the input .docx file.
    ranges : list of str
        Page ranges to extract, e.g. ["1", "2-13", "14-last"].
    output_path : str
        Directory where output PDFs will be saved.
    names : list of str, optional
        Names for each output PDF (without extension). Must match the length
        of `ranges`. If None, filenames default to "pages_{range}.pdf".
    """
    if names is not None and len(names) != len(ranges):
        raise ValueError(f"Length of 'names' ({len(names)}) must match length of 'ranges' ({len(ranges)}).")

    pdf_path = docx_path.replace(".docx", ".pdf")
    # Convert a single file
    convert(docx_path, pdf_path)
    
    # Create the output directory if it doesn't exist
    os.makedirs(output_path, exist_ok=True)
    
    reader = PdfReader(pdf_path)
    total_pages = len(reader.pages)
    
    for i, r in enumerate(ranges):
        writer = PdfWriter()
        
        # Handle single pages (e.g., "1")
        if "-" not in r:
            start = int(r)
            end = start
        else:
            # Handle ranges (e.g., "2-13" or "14-last")
            start_str, end_str = r.split("-")
            start = int(start_str)
            
            if end_str.lower() == "last":
                end = total_pages
            else:
                end = int(end_str)
        
        # Extract pages (convert 1-based indexing to 0-based indexing)
        for page_num in range(start - 1, end):
            writer.add_page(reader.pages[page_num])
        
        # Use provided name or fall back to default
        filename = f"{names[i]}.pdf" if names is not None else f"pages_{r}.pdf"
        output_filename = os.path.join(output_path, filename)
        
        with open(output_filename, "wb") as f:
            writer.write(f)
        print(f"Created: {output_filename}")
