FROM python:3.10-slim-bookworm

WORKDIR /app

COPY requirements.txt .
COPY src/*.py .

RUN pip install --upgrade pip
RUN pip install -r requirements.txt

EXPOSE 8000

CMD ["python", "/app/main.py"]