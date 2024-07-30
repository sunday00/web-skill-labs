package local.sunday00.ytbspringmongo.exceptions;

import graphql.ErrorClassification;

public enum CustomErrorType implements ErrorClassification {
    BAD_REQUEST,
    UNAUTHORIZED,
    FORBIDDEN,
    NOT_FOUND,
    INTERNAL_ERROR,
    FORCED_INTENDED_ERROR
}
