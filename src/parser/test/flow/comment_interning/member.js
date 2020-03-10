const x = {y: 0};

function staticMemberExpression() {
  /* 1.1 L id */ x /* 1.2 T id */ . /* 1.3 L id */ y /* 1.4 T id */;

  /* 2.1 L member */ (/* 2.2 L id */ x.y /* 2.3 T id */) /* 2.4 T member */;
}

function dynamicMemberExpression() {
  /* 3.1 L id */ x /* 3.2 T id */ [/* 3.3 L id */ y /* 3.4 T id */] /* 3.5 T member */;

  /* 4.1 L member */ (/* 4.2 L id */ x[y] /* 4.3 T member */) /* 4.4 T member */;
}
