# Add Components

- app / components

## Box, Flex

- ChakraUi 에 영김을 받은 편의 정렬 wrapper div
- box는 세로로 쌓아올린 레이아웃에서 자식들 간 간격을 자동 확보
- flex는 기본적으로 가로로 배열할 때 justify-between

## button, closeBtn

- 모양 일관성을 위해 임시 생성
- tailwindcss, daisyui 특성상 className이 지나치게 장황해지는 경향이 있으므로 되도록 적극 활용

## select

- browser 기본 select 박스는 option 꾸미기가 제한적이라 일단 만들어 둠.
- 디자인에 따라 수정 필요할 수 있고, built in select box 기능을 최대한 수동 구현해 두었으나 필요하면 js 수정

### options

- select 후보 options 들
- { show?: string; value: string | number }[]
    - show: 노출되는 값. 사용자가 화면에서 읽을 값
    - value: 실제 form을 통해 전송되는 값
    - show 가 없으면 value 기반으로 show에도 노출

### defaultSelect

- 처음 선택되어져 있는 값
- { show?: string; value: string | number }
- 없으면 options 의 첫 값
- update form 등 기존에 선택한 값이 있을때 지정 필요

## pagination

- pagination은 무한스크롤 아닌 전통적 10개단위 버튼식 이동 페이지 버튼 반복작업을 피하고 모양 일관성을 위해 구현하였다.
- 통상 CommonList<T> 의 응답에서 {items: T[], total, page, size} 등을 한 세트로 응답하게 해두었으므로, 이 세트를 통째로 bulk 로 넣으면 된다.

```typescript jsx

export default function Component() {
...
  return (
    <>
      <ul> {
        article.items.map(item => <li content={item} key={item.id} />)
      } </ul>

      <Paginate bulk={articles} />
    </>
  )
}

```

# css

## scrollbar

- mac 에선 scrollbar 가 자동으로 감춰지는데,
- 내용이 더 있음이 잘 드러나지 않을 때 강제 노출

```typescript jsx
<div
  className={'w-full border show-scroll-bar'} // like this
  style={{
    overflowY: 'scroll',
    height: '20em',
  }}
>
  <div
    draggable
    className={'bg-yellow-300 w-1/3 mx-auto'}
    style={{
      height: '60em',
    }}
  >
    Example
  </div>
</div>
```

## className

- tailwindcss 는 className 기반으로 동작하고 별도의 style={{}}이 필요한 경우는 극히 적을 것으로 예상
- color 를 사용할 때는 daisyui를 참고하여 text-primary, text-base-content, bg-secondary 등을 활용하기 바람.

```typescript jsx
<div className={'bg-pink-400'} /> // X

//

<div className={'bg-accent'} /> // O

```

- 중소기업 특성상 갑자기 core color 를 한꺼번에 ~로 바꾸어 달라거나, 추후 dark mode 등이 되었을떄 theme 적용하려면 색을 통일화 해서 써야 함.
- 테마와 색 이름 추가가 필요한 경우에는 tailwind.config.ts 참고