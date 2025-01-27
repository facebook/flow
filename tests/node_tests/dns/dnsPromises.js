var dns = require("dns");

/* lookup */
async function lookup() {
  type LookupResult = {address: string, family: number};

  const result1: LookupResult = await dns.lookup("test.com");
  const result2: LookupResult = await dns.lookup("test.com", 6);
  const result2: LookupResult = await dns.lookup("test.com", {family: 6});

  // errors
  await dns.lookup(); // error, hostname expected
  await dns.lookup("test.com", (err, address, family) => {}); // error, no callback expected
  await dns.lookup("test.com", null); // second param can't be null
}


