import { useRef } from 'react';
import ReactCropper, { ReactCropperElement } from 'react-cropper';
import 'cropperjs/dist/cropper.css';

export const Preview = ({ src }: { src: string }) => {
  const cropperRef = useRef<ReactCropperElement>(null);
  const onCrop = async () => {
    const cropper = cropperRef.current?.cropper;
    const result = await fetch('http://localhost:3003/api/v20231013/b64', {
      method: 'POST',
      headers: {
        Accept: 'application/json',
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        title: 'test',
        files: [cropper?.getCroppedCanvas().toDataURL()],
      }),
    });

    console.log(result);
  };

  return (
    <>
      <ReactCropper
        ref={cropperRef}
        style={{ width: 300 }}
        initialAspectRatio={1}
        src={src}
        background={false}
        responsive={true}
        zoomable={false}
        guides={true}
      />
      <button onClick={onCrop}>Crop</button>
    </>
  );
};
