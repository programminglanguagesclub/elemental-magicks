var flames22Image = new Image();
flames22Image.src = "data:image/png;base64,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"