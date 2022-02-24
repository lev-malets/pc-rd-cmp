
$(clone angstrom,'https://github.com/inhabitedtype/angstrom',b0e7849ec59746b44753a4c5f5954ba95d1c6c5a)
$(patch angstrom,$D/angstrom.patch)

$(clone bark,'https://github.com/justinlubin/bark',8586a2796361d54009c2f942e24fd5aad71f1cd0)
$(patch bark,$D/bark.patch)

$(clone mparser,'https://github.com/murmour/mparser',826fc1d9c68b5cafc0b2637ad395226f1a5abfa0)
$(patch mparser,$D/mparser.patch)

$(clone opal,'https://github.com/pyrocat101/opal',8d8a1f3d248610f5ce4890951ea0ffb1b143d39b)
$(patch opal,$D/opal.patch)

$(clone syntax,'https://github.com/rescript-lang/syntax',d9c44bb22892dc4e77334f064d8bccd119fecde9)
$(patch syntax,$D/syntax.patch)

$K/done: $K/patch/angstrom $K/patch/bark $K/patch/mparser $K/patch/opal $K/patch/syntax
