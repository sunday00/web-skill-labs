import { Box } from '@chakra-ui/react'
import CardNav from '@/components/CardNav'
import logo from '@/assets/react.svg'
import AnimatedList from '@/components/AnimatedList.tsx'
import CardSwap, { Card } from '@/components/CardSwap.tsx'
import Counter from '@/components/Counter.tsx'
import { useEffect, useState } from 'react'
import DecryptedText from '@/components/DecryptedText.tsx'
import LetterGlitch from '@/components/LetterGlitch.tsx'
import Hyperspeed from '@/components/Hyperspeed.tsx'

const ReactBitsMenu = () => {
  const [count, setCount] = useState(0)
  const [letters, setLetters] = useState('       ')

  useEffect(() => {
    setTimeout(() => {
      setCount(123878219)
      setLetters(' alphaka states hacked ')
    }, 10)
  }, [])

  const items = [
    {
      label: 'About',
      bgColor: '#0D0716',
      textColor: '#fff',
      links: [
        { label: 'Company', ariaLabel: 'About Company' },
        { label: 'Careers', ariaLabel: 'About Careers' },
      ],
    },
    {
      label: 'Projects',
      bgColor: '#170D27',
      textColor: '#fff',
      links: [
        { label: 'Featured', ariaLabel: 'Featured Projects' },
        { label: 'Case Studies', ariaLabel: 'Project Case Studies' },
      ],
    },
    {
      label: 'Contact',
      bgColor: '#271E37',
      textColor: '#fff',
      links: [
        { label: 'Email', ariaLabel: 'Email us' },
        { label: 'Twitter', ariaLabel: 'Twitter' },
        { label: 'LinkedIn', ariaLabel: 'LinkedIn' },
      ],
    },
  ]

  const listItems = ['apple', 'banana', 'cherry']

  return (
    <Box className={'wrap-module-box'}>
      <CardNav
        logo={logo}
        logoAlt="Company Logo"
        items={items}
        baseColor="#fff"
        menuColor="#000"
        buttonBgColor="#111"
        buttonTextColor="#fff"
        ease="power3.out"
      />

      <AnimatedList
        items={listItems}
        onItemSelect={(item, index) => console.log(item, index)}
        showGradients={true}
        enableArrowNavigation={true}
        displayScrollbar={true}
      />

      <Box>hm...?</Box>

      <Box mb={'8em'}>
        <CardSwap
          cardDistance={60}
          verticalDistance={70}
          delay={2000}
          pauseOnHover={false}
        >
          <Card>
            <h3>Card 1</h3>
            <p>Your content here</p>
          </Card>
          <Card>
            <h3>Card 2</h3>
            <p>Your content here</p>
          </Card>
          <Card>
            <h3>Card 3</h3>
            <p>Your content here</p>
          </Card>
        </CardSwap>
      </Box>

      <Box>hm...?</Box>

      <Counter value={count} />

      <Box>
        Hellllo.... this file
        <DecryptedText
          text={letters}
          speed={100}
          maxIterations={20}
          characters="ABCD1234!?"
          // className="revealed"
          parentClassName="all-letters"
          encryptedClassName="encrypted"
        />
      </Box>

      <Box style={{ marginTop: '4rem' }}>
        <DecryptedText
          text="This text animates when in view"
          animateOn="view"
          revealDirection="center"
        />
      </Box>

      <LetterGlitch
        glitchSpeed={50}
        centerVignette={true}
        outerVignette={false}
        smooth={true}
        characters={'ㄱㄴㄷㄹ강감찬핫도그해돋이'}
        glitchColors={['#3f2b45', '#9261dc', '#dc61ab']}
      />

      <Hyperspeed
        effectOptions={{
          onSpeedUp: () => {},
          onSlowDown: () => {},
          distortion: 'turbulentDistortion',
          length: 400,
          roadWidth: 10,
          islandWidth: 2,
          lanesPerRoad: 4,
          fov: 90,
          fovSpeedUp: 150,
          speedUp: 2,
          carLightsFade: 0.4,
          totalSideLightSticks: 20,
          lightPairsPerRoadWay: 40,
          shoulderLinesWidthPercentage: 0.05,
          brokenLinesWidthPercentage: 0.1,
          brokenLinesLengthPercentage: 0.5,
          lightStickWidth: [0.12, 0.5],
          lightStickHeight: [1.3, 1.7],
          movingAwaySpeed: [-60, -80],
          movingCloserSpeed: [120, 160],
          carLightsLength: [400 * 0.03, 400 * 0.2],
          carLightsRadius: [0.05, 0.14],
          carWidthPercentage: [0.3, 0.5],
          carShiftX: [-0.8, 0.8],
          carFloorSeparation: [0, 5],
          colors: {
            roadColor: 0x080808,
            islandColor: 0x0a0a0a,
            background: 0x000000,
            shoulderLines: 0xffffff,
            brokenLines: 0xffffff,
            rightCars: [0xd856bf, 0x6750a2, 0xc247ac],
            leftCars: [0x03b3c3, 0x0e5ea5, 0x324555],
            sticks: 0x03b3c3,
          },
        }}
      />
    </Box>
  )
}

export default ReactBitsMenu
