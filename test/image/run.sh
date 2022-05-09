set -e

K=$1
NAME=$(cat $K/name)

echo Create volume for repo
VOLUME=$(docker volume create)
echo Created volume: $VOLUME
echo Run cp container
CONTAINER_CP=$(docker run -d -v $VOLUME:/repo alpine sleep infinity)
echo Started cp container: $CONTAINER_CP
echo Copy repo files
docker cp . $CONTAINER_CP:/repo
echo Change repo files ownership
docker exec $CONTAINER_CP chown -R 0:0 /repo
echo Remove cp container
docker rm -f $CONTAINER_CP
echo Start test container
docker run --rm --tmpfs /tmp -v $VOLUME:/repo $NAME
