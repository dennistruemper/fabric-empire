# Build stage
FROM node:22-alpine AS builder

WORKDIR /app

# Copy package files
COPY package.json package-lock.json ./
COPY elm.json elm-tooling.json ./

# Install dependencies using npm ci
# --ignore-scripts prevents elm npm package installation (via vite-plugin-elm-watch -> elm)
# which is not supported on ARM64. elm-tooling handles Elm installation properly.
RUN npm ci --ignore-scripts

# Copy elm-land config, source files, static assets, and SW generator
COPY elm-land.json ./
COPY src/ ./src/
COPY static/ ./static/
COPY scripts/generate-sw.mjs ./scripts/generate-sw.mjs

# Install elm tooling and build the application (includes dist/sw.js)
RUN npx elm-tooling install
RUN npm run build

# Production stage
FROM nginx:alpine

# Copy custom nginx config for SPA routing + caching + gzip
COPY nginx.conf /etc/nginx/conf.d/default.conf

# Copy the built static files from the builder stage
COPY --from=builder /app/dist/ /usr/share/nginx/html/

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
