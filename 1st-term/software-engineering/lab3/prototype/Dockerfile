FROM python:3.12-slim

WORKDIR /app

COPY requirements.txt /app/

RUN apt update \
	&& \
	apt install -y --no-install-recommends build-essential python3-cffi python3-brotli libpango-1.0-0 libpangoft2-1.0-0 \
	&& \
	python3 -m pip install --no-cache-dir -r requirements.txt \
	&& \
	apt purge -y build-essential

COPY src src

WORKDIR /app/src

CMD ["python3", "app.py"]
