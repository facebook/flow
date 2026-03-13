// Test that Date comparison works when the Date is narrowed from
// a nullable type after reassignment. The reason description gets
// wrapped (e.g., in RUnionBranching), and name_of_instance_reason
// must unwrap it to recognize "Date" for the comparison check.

const NOW = new Date();
const TODAY = new Date(NOW.getFullYear(), NOW.getMonth(), NOW.getDate());

function getStartDate(raw: ?number): ?Date {
  if (raw != null) {
    return new Date(raw);
  }
  return null;
}

const raw: ?number = 12345;
let startDate = getStartDate(raw);
if (startDate != null) {
  startDate = new Date(
    startDate.getFullYear(),
    startDate.getMonth(),
    startDate.getDate(),
  );
}

// Date comparison should work even when Date flows through narrowing
if (startDate != null && TODAY > startDate) {} // ok
