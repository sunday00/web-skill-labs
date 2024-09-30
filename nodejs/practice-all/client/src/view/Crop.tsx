import { useState } from 'react';
// import { ReactCrop } from 'react-image-crop';
import { FileUploader } from 'react-drag-drop-files';
import { Preview } from '../component/crop/Preview.tsx';

const fileTypesUpper = ['JPG', 'JPEG', 'PNG', 'GIF'];
const fileTypes = [
  ...fileTypesUpper,
  ...fileTypesUpper.map((f) => f.toLowerCase()),
];

export const Crop = () => {
  const [file, setFile] = useState([] as unknown as File[]);
  const handleChange = (uploadFile: File[]) => {
    setFile([...file, ...uploadFile]);
  };

  const preview = file.map((f, i) => {
    return <Preview src={URL.createObjectURL(f)} key={i} />;
  });

  return (
    <>
      <FileUploader
        handleChange={handleChange}
        name="file"
        types={fileTypes}
        multiple={true}
      />

      {preview}
    </>
  );
};
