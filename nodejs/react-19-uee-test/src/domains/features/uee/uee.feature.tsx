import { useEffect, useEffectEvent, useRef, useState } from 'react'
import { Input } from '@/components/ui/input.tsx'
import { Field, FieldDescription, FieldLabel } from '@/components/ui/field.tsx'
import { useInterval } from '@/hooks/useInterval.hook.tsx'

const UEE = () => {
  // -------------------- --------------- --------------- ---------------

  const [userName, setUserName] = useState('Bob')
  const [loginMessage, setLoginMessage] = useState('')

  useEffect(() => {
    let loggedInTime = 0
    const interval = setInterval(() => {
      loggedInTime++

      setLoginMessage(
        `${userName} has been logged in for ${loggedInTime} seconds`,
      )
    }, 1000)
    return () => clearInterval(interval)
  }, [userName])

  // -------------------- --------------- --------------- ---------------

  const nameRef = useRef(userName)
  nameRef.current = userName

  const [userName2, setUserName2] = useState('Bob')
  const [loginMessage2, setLoginMessage2] = useState('')

  useEffect(() => {
    let loggedInTime = 0
    const interval = setInterval(() => {
      loggedInTime++

      setLoginMessage2(
        `${nameRef.current} has been logged in for ${loggedInTime} seconds`,
      )
    }, 1000)
    return () => clearInterval(interval)
  }, [])

  // -------------------- --------------- --------------- ---------------

  const [userName3, setUserName3] = useState('Bob')
  const [loginMessage3, setLoginMessage3] = useState('')

  const getName = useEffectEvent(() => userName3)

  useEffect(() => {
    let loggedInTime = 0
    const interval = setInterval(() => {
      loggedInTime++

      setLoginMessage3(
        `${getName()} has been logged in for ${loggedInTime} seconds`,
      )
    }, 1000)
    return () => clearInterval(interval)
  }, [])

  // -------------------- --------------- --------------- ---------------

  const [userName4, setUserName4] = useState('Bob')
  const [loginMessage4, setLoginMessage4] = useState('')

  const onTick = useEffectEvent((tick: number) => {
    setLoginMessage4(`${userName4} has been logged in for ${tick} seconds`)
  })

  useEffect(() => {
    let tick = 0
    const interval = setInterval(() => onTick(++tick), 1000)
    return () => clearInterval(interval)
  }, [])

  // -------------------- --------------- --------------- ---------------

  const [userName5, setUserName5] = useState('Bob')
  const [loginMessage5, setLoginMessage5] = useState('')

  useInterval((tick) => {
    setLoginMessage5(`${userName5} has been logged in for ${tick} seconds`)
  })

  // -------------------- --------------- --------------- ---------------

  const [userName6, setUserName6] = useState('Bob')
  const [loginMessage6, setLoginMessage6] = useState('')

  useInterval((tick) => {
    setLoginMessage6(`${userName6} has been logged in for ${tick} seconds`)
  }, 200)

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
          <FieldDescription>Standard : {loginMessage}</FieldDescription>
        </Field>
      </div>

      <div className="my-4">
        <Field>
          <FieldLabel>UserName</FieldLabel>
          <Input
            type="text"
            placeholder="userName"
            value={userName2}
            onChange={(e) => setUserName2(e.target.value)}
          />
          <FieldDescription>UseRef way : {loginMessage2}</FieldDescription>
        </Field>
      </div>

      <div className="my-4">
        <Field>
          <FieldLabel>UserName</FieldLabel>
          <Input
            type="text"
            placeholder="userName"
            value={userName3}
            onChange={(e) => setUserName3(e.target.value)}
          />
          <FieldDescription>
            useEffectEvent way : {loginMessage3}
          </FieldDescription>
        </Field>
      </div>

      <div className="my-4">
        <Field>
          <FieldLabel>UserName</FieldLabel>
          <Input
            type="text"
            placeholder="userName"
            value={userName4}
            onChange={(e) => setUserName4(e.target.value)}
          />
          <FieldDescription>
            useEffectEvent + responsible separate way : {loginMessage4}
          </FieldDescription>
        </Field>
      </div>

      <div className="my-4">
        <Field>
          <FieldLabel>UserName</FieldLabel>
          <Input
            type="text"
            placeholder="userName"
            value={userName5}
            onChange={(e) => setUserName5(e.target.value)}
          />
          <FieldDescription>
            useEffectEvent custom hook : {loginMessage5}
          </FieldDescription>
        </Field>
      </div>

      <div className="my-4">
        <Field>
          <FieldLabel>UserName</FieldLabel>
          <Input
            type="text"
            placeholder="userName"
            value={userName6}
            onChange={(e) => setUserName6(e.target.value)}
          />
          <FieldDescription>
            useEffectEvent custom hook : {loginMessage6}
          </FieldDescription>
        </Field>
      </div>
    </section>
  )
}

export default UEE
