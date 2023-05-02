FROM node:20

WORKDIR /app
COPY package*.json .
RUN npm ci
COPY . .
RUN node_modules/.bin/tsc

CMD [ "node", "dist/index.js" ]