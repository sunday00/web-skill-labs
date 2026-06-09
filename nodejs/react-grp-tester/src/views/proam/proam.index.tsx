import { Navigate, Route, Routes } from 'react-router'
import { List, Separator } from '@chakra-ui/react'
import { ProamAttach } from '@/views/proam/attach/attach.index.tsx'
import { ProamAttachDynamic } from '@/views/proam/dynamic/dynamic.index.tsx'

const Proam = () => {
  return (
    <>
      <List.Root>
        <List.Item>
          <a href="/proam/attach">Attach</a>
        </List.Item>
        <List.Item>
          <a href="/proam/dynamic">DynamicAttach</a>
        </List.Item>
      </List.Root>

      <Separator my={8} />

      <Routes>
        <Route path="/attach/*" element={<ProamAttach />} />
        <Route path="/dynamic/*" element={<ProamAttachDynamic />} />

        <Route path="/" element={<Navigate to="/proam" replace />} />
      </Routes>
    </>
  )
}

export { Proam }
