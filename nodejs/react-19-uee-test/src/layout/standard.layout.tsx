import { Route, Routes } from 'react-router'
import Home from '@/domains/home/home.index.tsx'
import Feature from '@/domains/features/feature.index.tsx'
import LnbNav from '@/components/lnb/lnb.nav.tsx'

const StandardLayout = () => {
  const items = [{ id: '0', label: 'features', href: '/feature' }]

  return (
    <div className={'wrap'}>
      <section>
        <h1 className="text-2xl font-bold mb-4">
          RND - react 19 - tailwind - shardcn{' '}
        </h1>

        <section>
          <LnbNav items={items} />
        </section>
      </section>

      <Routes>
        <Route path="/feature/*" element={<Feature />}></Route>
        <Route path="/home/*" element={<Home />}></Route>
      </Routes>
    </div>
  )
}

export default StandardLayout
