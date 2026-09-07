import "./tracing";
import express from "express";

const app = express();

app.get("/", (_req, res) => {
  res.send("hello");
});

app.get("/work", (_req, res) => {
  const sum = Array.from({ length: 1_000_000 }, (_, i) => i).reduce((a, b) => a + b, 0);
  res.json({ sum });
});

const PORT = process.env.PORT ?? 3939;
app.listen(PORT, () => {
  console.log(`listening on http://localhost:${PORT}`);
});
