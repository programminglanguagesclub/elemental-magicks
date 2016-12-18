var flames23Image = new Image();
flames23Image.src = "data:image/png;base64,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"