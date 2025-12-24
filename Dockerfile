FROM python:3.11-slim

WORKDIR /app

# Copy and install Python dependencies (from root)
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy your app code (from app/ directory)
COPY app/app.py .

# Expose port 7860 (HuggingFace Spaces requirement)
EXPOSE 7860

# Run Streamlit on port 7860
CMD ["streamlit", "run", "app.py", \
     "--server.port=7860", \
     "--server.address=0.0.0.0", \
     "--server.headless=true"]