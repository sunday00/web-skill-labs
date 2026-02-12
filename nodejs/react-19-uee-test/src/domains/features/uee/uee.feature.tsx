import { useState } from 'react'
import { Input } from '@/components/ui/input.tsx'
import { Field, FieldDescription, FieldLabel } from '@/components/ui/field.tsx'

const UEE = () => {
  const [userName, setUserName] = useState('Bob')

  return (
    <section>
      <h3 className="text-lg mt-8">UEE</h3>

      <hr />

      <div className="my-4">
        <Field>
          <FieldLabel>UserName</FieldLabel>
          <Input
            type="text"
            placeholder="userName"
            value={userName}
            onChange={(e) => setUserName(e.target.value)}
          />
          <FieldDescription>Standard</FieldDescription>
        </Field>
      </div>
    </section>
  )
}

export default UEE
