# Runtime image for the Sudoku backend (sudoku.matsuo.pl).
#
# The application is a single self-contained executable JAR produced by the elm-lang compiler:
#   java -jar elm.jar bundle server server/SudokuServer.elm --port 8080 -o server/sudoku
# which embeds the Elm source + interpreter (Truffle runs in its portable fallback mode on a stock
# JRE — fine for this low-traffic API). The CI workflow builds that jar and drops it next to this
# Dockerfile, so the image build is just a copy. See .github/workflows/deploy-server.yml.
FROM eclipse-temurin:25-jre

WORKDIR /app
COPY sudoku.jar /app/sudoku.jar

# The port is baked into the jar (--port 8080); nginx-proxy routes VIRTUAL_HOST here on VIRTUAL_PORT.
EXPOSE 8080

# The two flags just silence GraalVM/Truffle's start-up notices (native-access + sun.misc.Unsafe);
# the server runs without them too.
ENTRYPOINT ["java", "--enable-native-access=ALL-UNNAMED", "--sun-misc-unsafe-memory-access=allow", "-jar", "/app/sudoku.jar"]
