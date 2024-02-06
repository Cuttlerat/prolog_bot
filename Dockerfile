FROM swipl:8.1.8

LABEL version="0.1.0"

WORKDIR /app
COPY ./bot /app

ENTRYPOINT ["swipl"]

CMD ["bot.pl"]
