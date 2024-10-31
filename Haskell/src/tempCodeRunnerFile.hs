parseFreeTextChunk :: Parser FreeTextorModifier
parseFreeTextChunk = do
    content <- many (noneof "_*~`[")
    moreContent <- optional parseFakeModifierSymbol
    let fullContent = case moreContent of
                        Just extra -> content ++ extra
                        Nothing    -> content
    pure (FreeTextChunk fullContent)