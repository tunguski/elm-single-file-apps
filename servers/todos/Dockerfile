# Runtime image for the todos backend (todos.matsuo.pl). The CI workflow bundles the server into a
# self-contained jar and builds the todos single-file app; both are dropped next to this Dockerfile.
FROM eclipse-temurin:25-jre
WORKDIR /app
COPY server.jar /app/server.jar
COPY index.html /app/public/index.html
# H2 file lives here (persisted via the todos-db volume in production); STATIC_DIR serves the app UI.
RUN mkdir -p /app/db
ENV STATIC_DIR=/app/public
EXPOSE 8080
ENTRYPOINT ["java", "--enable-native-access=ALL-UNNAMED", "--sun-misc-unsafe-memory-access=allow", "-jar", "/app/server.jar"]
