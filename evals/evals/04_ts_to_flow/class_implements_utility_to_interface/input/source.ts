type Document = {
  id: string;
  title: string;
  body: string;
  wordCount: number;
  secret: string;
};

class PublicDocument implements Omit<Document, "secret"> {
  id: string;
  title: string;
  body: string;
  wordCount: number;

  constructor(raw: {id: string; title: string; body: string}) {
    this.id = raw.id;
    this.title = raw.title.trim();
    this.body = raw.body;
    this.wordCount = raw.body.trim().split(/\s+/).filter(w => w.length > 0).length;
  }

  excerpt(maxChars: number): string {
    if (this.body.length <= maxChars) {
      return this.body;
    }
    return this.body.slice(0, Math.max(0, maxChars - 1)) + "…";
  }
}

const doc = new PublicDocument({
  id: "d-42",
  title: "  Weekly Update  ",
  body: "Shipped three fixes and updated the docs page on Wednesday.",
});
console.log(`${doc.id} :: ${doc.title} (${doc.wordCount} words)`);
console.log(doc.excerpt(30));
