import '@/css/scrollbar.css'

export default function Plain() {
  return (
    <>
      <div
        className={'w-full border show-scroll-bar'}
        style={{
          overflowY: 'scroll',
          height: '20em',
        }}
      >
        <div
          className={'bg-yellow-300 w-1/3 mx-auto'}
          style={{
            height: '60em',
          }}
        >
          Example
        </div>
      </div>
    </>
  )
}
