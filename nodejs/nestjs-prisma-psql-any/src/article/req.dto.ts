import { Article } from '@prisma/client';
import { ApiProperty } from '@nestjs/swagger';

export class ArticleCreateDto implements Partial<Article> {
  @ApiProperty()
  title: string;

  @ApiProperty()
  slug: string;

  @ApiProperty()
  content: string;
}
