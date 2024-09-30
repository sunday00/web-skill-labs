import './App.css';
import { Outlet } from 'react-router-dom';
import { Nav } from './component/Nav.tsx';

function App() {
  return (
    <>
      <Nav />
      <Outlet />
    </>
  );
}

export default App;
