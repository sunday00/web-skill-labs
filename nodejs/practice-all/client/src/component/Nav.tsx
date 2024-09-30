import { createBrowserRouter, Link } from 'react-router-dom';
import App from '../App.tsx';
import { Home } from '../view/Home.tsx';
import { Crop } from '../view/Crop.tsx';

/**
 * @CONF: add here to more subjects
 * @description not need to update router, nav.
 * */
const prefix = '/client';
const routes = {
  path: prefix,
  element: <App />,
  children: [
    { index: true, name: 'home', path: prefix + '/', element: <Home /> },
    { name: 'crop', path: `${prefix}/crop`, element: <Crop /> },
  ],
};

// INFO: router
export const router = createBrowserRouter([routes]);

// INFO: nav
export const Nav = () => {
  return (
    <nav style={{ display: 'flex', justifyContent: 'start', gap: '1em' }}>
      {routes.children.map((r, i) => (
        <Link key={i} to={r.path}>
          {r.name}
        </Link>
      ))}
    </nav>
  );
};
