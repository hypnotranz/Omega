// Start test server on port 3457
import { startDebugServer } from './src/server/index';

const port = process.env.PORT ? parseInt(process.env.PORT) : 3456;
console.log(`Starting debug server on port ${port}...`);
startDebugServer(port).then((server) => {
  console.log(`Server started on http://localhost:${port}`);
}).catch((err) => {
  console.error('Failed to start server:', err);
  process.exit(1);
});
