import { Route, Routes } from 'react-router'
import UEE from '@/domains/features/uee/uee.feature.tsx'
import LnbNav from '@/components/lnb/lnb.nav.tsx'

const Feature = () => {
  const items = [{ id: '0', label: 'useEffectEvent', href: '/feature/uee' }]

  return (
    <section className={'domain-feature-wrap'}>
      <h2 className="text-xl font-bold mb-4">react 19 features</h2>

      <section className={'index'}>
        <LnbNav items={items} />
      </section>

      <Routes>
        <Route path="/uee" element={<UEE />} />
      </Routes>
    </section>
  )
}

export default Feature
