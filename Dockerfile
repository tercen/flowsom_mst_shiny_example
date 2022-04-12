FROM tercen/flowsuite:0.0.3

COPY . /operator
WORKDIR /operator

ENV TERCEN_SERVICE_URI https://tercen.com

COPY start.R /start.R

ENTRYPOINT [ "R","--no-save","--no-restore","--no-environ","--slave","-f","/start.R"]