NAME=$(cat $1)

echo Create volume for repo
VOLUME=$(docker volume create)
echo Created volume: $VOLUME
echo Run cp container
CONTAINER_CP=$(docker run -d -v $VOLUME:/repo alpine sleep infinity)
echo Started cp container: $CONTAINER_CP
echo Copy repo files
docker cp . $CONTAINER_CP:/repo
echo Change repo files ownership
docker exec $CONTAINER_CP chown -R 1000 /repo
echo Remove cp container
docker rm -f $CONTAINER_CP
echo Run test container
docker run --rm --tmpfs /tmp -v $VOLUME:/repo \
    -w /repo \
    -u user \
    $NAME sh -c 'eval $(opam env) && make test'
echo Delete volume
docker volume rm $VOLUME
