import {
  Body,
  Controller,
  Get,
  Param,
  ParseIntPipe,
  Post,
  Query,
} from '@nestjs/common';
import { ArticleService } from './article.service';
import { ApiTags } from '@nestjs/swagger';
import { ArticleCreateDto } from './req.dto';

@ApiTags('article')
@Controller('article')
export class ArticleController {
  constructor(private readonly articleService: ArticleService) {}

  @Get('/')
  async getArticles(@Query('id', ParseIntPipe) id: number) {
    return this.articleService.getArticle(id);
  }

  @Post('/')
  async createArticle(@Body() body: ArticleCreateDto) {
    return this.articleService.createArticle(body);
  }

  @Get('/unsafe/:id')
  async getArticlesUnsafe(@Param('id') id: string) {
    return this.articleService.getArticleUnsafe(id as unknown as number);
  }

  @Get('/safe/:id')
  async getArticlesSafe(@Param('id') id: string) {
    return this.articleService.getArticleSafe(id as unknown as number);
  }
}
