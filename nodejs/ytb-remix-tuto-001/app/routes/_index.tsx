import type { MetaFunction } from "@remix-run/node";
import {
  Button,
  Fieldset,
  Flex,
  Input,
  NativeSelect,
  Stack,
} from "@chakra-ui/react";
import { Field } from "~/components/ui/field";
import theme from "tailwindcss/defaultTheme";

export const meta: MetaFunction = () => {
  return [
    { title: "New Remix App" },
    { name: "description", content: "Welcome to Remix!" },
  ];
};

export default function Index() {
  return (
    <div className="flex h-screen items-center justify-center">
      <Flex justify={"center"} gap={theme.spacing["4"]} bg={"whiteAlpha.200"}>
        <Fieldset.Root size="lg" maxW="md">
          <Stack>
            <Fieldset.Legend>Contact details</Fieldset.Legend>
            <Fieldset.HelperText>
              Please provide your contact details below.
            </Fieldset.HelperText>
          </Stack>

          <Fieldset.Content>
            <Field label="Name">
              <Input name="name" />
            </Field>

            <Field label="Email address">
              <Input name="email" type="email" />
            </Field>

            <Field label="Country">
              <NativeSelect.Root>
                <NativeSelect.Field name="country">
                  {[
                    "United Kingdom (UK)",
                    "Canada (CA)",
                    "United States (US)",
                  ].map((s, idx) => (
                    <option value={idx} key={idx}>
                      {s}
                    </option>
                  ))}
                </NativeSelect.Field>
              </NativeSelect.Root>
            </Field>
          </Fieldset.Content>

          <Button type="submit" alignSelf="flex-start">
            Submit
          </Button>
        </Fieldset.Root>
      </Flex>
    </div>
  );
}
