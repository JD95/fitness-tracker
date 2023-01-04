./build-backend-deps-image.ps1
docker build --no-cache --network="host" -t fitness-tracker:latest -f Server.Dockerfile .