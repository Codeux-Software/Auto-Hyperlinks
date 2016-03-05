/*
 * The AutoHyperlinks Framework is the legal property of its developers (DEVELOPERS), 
 * whose names are listed in the copyright file included with this source distribution.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the AutoHyperlinks Framework nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY ITS DEVELOPERS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL ITS DEVELOPERS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

typedef void					*yyscan_t;
typedef struct					AH_buffer_state *AH_BUFFER_STATE;

extern long						AHlex(yyscan_t yyscanner);
extern long						AHlex_init(yyscan_t *ptr_yy_globals);
extern long						AHlex_destroy(yyscan_t yyscanner);
extern long						AHget_leng(yyscan_t scanner);
extern void						AHset_in(FILE *in_str, yyscan_t scanner);
extern void						AH_switch_to_buffer(AH_BUFFER_STATE, yyscan_t scanner);
extern void						AH_delete_buffer(AH_BUFFER_STATE, yyscan_t scanner);
extern YY_EXTRA_TYPE			AHget_extra(yyscan_t scanner);
extern AH_BUFFER_STATE			AH_scan_string(const char *, yyscan_t scanner);

#define DEFAULT_URL_SCHEME	@"http://"

#define ENC_INDEX_KEY		@"encIndex"
#define ENC_CHAR_KEY		@"encChar"

#define MIN_LINK_LENGTH 4

@interface AHHyperlinkScanner (Private)
- (NSArray *)_allMatches;
- (NSRange)_longestBalancedEnclosureInRange:(NSRange)inRange;
+ (NSString *)_URLWithProperScheme:(NSString *)url parserStatus:(AHParserStatus)parserStatus;
- (NSArray *)_returnedValueWithProperURLScheme:(NSString *)scannedString inRange:(NSRange)scannedRange parserStatus:(AHParserStatus)parserStatus;
- (BOOL)_scanString:(NSString *)inString charactersFromSet:(NSCharacterSet *)inCharSet intoRange:(NSRange *)outRangeRef fromIndex:(unsigned long *)idx;
- (BOOL)_scanString:(NSString *)inString upToCharactersFromSet:(NSCharacterSet *)inCharSet intoRange:(NSRange *)outRangeRef fromIndex:(unsigned long *)idx;
@end

@implementation AHHyperlinkScanner
{
	NSString			*__weak m_scanString;
	
	BOOL				m_strictChecking;
	
	unsigned long		m_scanLocation;
	unsigned long		m_scanStringLength;

	NSMutableArray     *m_openEnclosureStack;
}

static NSCharacterSet			*skipSet						= nil;
static NSCharacterSet			*endSet							= nil;
static NSCharacterSet			*startSet						= nil;
static NSCharacterSet			*puncSet						= nil;
static NSCharacterSet			*hostnameComponentSeparatorSet	= nil;
static NSArray					*enclosureStartArray			= nil;
static NSCharacterSet			*enclosureSet					= nil;
static NSArray					*enclosureStopArray				= nil;
static NSArray					*encKeys						= nil;

@synthesize scanString			= m_scanString;
@synthesize strictChecking		= m_strictChecking;
@synthesize scanLocation		= m_scanLocation;
@synthesize scanStringLength	= m_scanStringLength;

#pragma mark -
#pragma mark Initalization

+ (void)initialize
{
	if (self == [AHHyperlinkScanner class]) {
		NSMutableCharacterSet *mutablePuncSet = [NSMutableCharacterSet new];
		NSMutableCharacterSet *mutableSkipSet = [NSMutableCharacterSet new];
		NSMutableCharacterSet *mutableStartSet = [NSMutableCharacterSet new];
		
		[mutableSkipSet formUnionWithCharacterSet:[NSCharacterSet whitespaceCharacterSet]];
		[mutableSkipSet formUnionWithCharacterSet:[NSCharacterSet illegalCharacterSet]];
		[mutableSkipSet formUnionWithCharacterSet:[NSCharacterSet controlCharacterSet]];
		[mutableSkipSet formUnionWithCharacterSet:[NSCharacterSet characterSetWithCharactersInString:@"<>"]];
		
		[mutableStartSet formUnionWithCharacterSet:[NSCharacterSet whitespaceCharacterSet]];
		[mutableStartSet formUnionWithCharacterSet:[NSCharacterSet characterSetWithCharactersInString:@"\"'“”‘’.…,:;<?!-–—@"]];
		
		[mutablePuncSet formUnionWithCharacterSet:[NSCharacterSet whitespaceCharacterSet]];
		[mutablePuncSet formUnionWithCharacterSet:[NSCharacterSet characterSetWithCharactersInString:@"\"'“”‘’.…,:;–—<?!"]];
		
		skipSet = [NSCharacterSet characterSetWithBitmapRepresentation:[mutableSkipSet bitmapRepresentation]];
		startSet = [NSCharacterSet characterSetWithBitmapRepresentation:[mutableStartSet bitmapRepresentation]];
		puncSet = [NSCharacterSet characterSetWithBitmapRepresentation:[mutablePuncSet bitmapRepresentation]];
		endSet = [NSCharacterSet characterSetWithCharactersInString:@"\"'“”‘’,:;>)]}–—.…?!@"];

		hostnameComponentSeparatorSet = [NSCharacterSet characterSetWithCharactersInString:@"./"];

		enclosureStartArray = @[@"(",@"[",@"{"];
		enclosureSet = [NSCharacterSet characterSetWithCharactersInString:@"()[]{}"];
		enclosureStopArray = @[@")",@"]",@"}"];

		encKeys = @[ENC_INDEX_KEY, ENC_CHAR_KEY];
	}
}

+ (AHHyperlinkScanner *)linkScanner
{
	return [[self class] new];
}

- (NSArray *)matchesForString:(NSString *)inString
{
	m_strictChecking = NO;

	m_scanString = inString;
	m_scanStringLength = [m_scanString length];

	m_openEnclosureStack = [NSMutableArray new];

	return [self _allMatches];
}

- (NSArray *)strictMatchesForString:(NSString *)inString
{
	m_strictChecking = YES;

	m_scanString = inString;
	m_scanStringLength = [m_scanString length];

	m_openEnclosureStack = [NSMutableArray new];
	
	return [self _allMatches];
}

- (void)dealloc
{
	m_scanLocation = 0;
}

+ (NSString *)URLWithProperScheme:(NSString *)url
{
	AHParserStatus parserStatus = [AHHyperlinkScanner isStringValidURI:url usingStrict:NO fromIndex:NULL];

	if (parserStatus == AHParserInvalidURLStatus) {
		return nil;
	} else {
		return [AHHyperlinkScanner _URLWithProperScheme:url parserStatus:parserStatus];
	}
}

+ (AHParserStatus)isStringValidURI:(NSString *)inString usingStrict:(BOOL)useStrictChecking fromIndex:(unsigned long *)sIndex
{
	yyscan_t scanner;

	const char *inStringEnc = [inString cStringUsingEncoding:NSUTF8StringEncoding];

	if (inStringEnc == NULL) {
		return AHParserInvalidURLStatus;
	}
	
	unsigned long inStringEncLength = strlen(inStringEnc);
    
	AHlex_init(&scanner);
	
    AH_BUFFER_STATE buffer = AH_scan_string(inStringEnc, scanner);
	
    AHParserStatus validStatus = (AHParserStatus)AHlex(scanner);

	if (validStatus == AHParserURLWithWildcardSchemeStatus) {
		NSRange schemeColonRange = [inString rangeOfString:@":"];

		if (schemeColonRange.location != NSNotFound) {
			NSString *urlScheme = [inString substringToIndex:schemeColonRange.location];

			if ([AHHyperlinkScanner isPermittedScheme:urlScheme] == NO) {
				validStatus = AHParserInvalidURLStatus;
			}
		}
	}

	if ((validStatus == AHParserURLWithRecognizedSchemeStatus ||
		 validStatus == AHParserURLWithWildcardSchemeStatus ||
		(validStatus == AHParserURLWithoutSchemeStatus && useStrictChecking == NO) ||
		(validStatus == AHParserURLIsSpecialCaseStatus_Reddit && useStrictChecking == NO)))
	{
        AH_delete_buffer(buffer, scanner);
		
        buffer = NULL;
        
		if (AHget_leng(scanner) == inStringEncLength) {
			if ( sIndex)
				*sIndex += [inString length];

			AHlex_destroy(scanner);
			
            return validStatus;
        }
    } else {
        AH_delete_buffer(buffer, scanner);
		
        buffer = NULL;
	}

	if ( sIndex)
		*sIndex += AHget_leng(scanner);

	AHlex_destroy(scanner);

	return AHParserInvalidURLStatus;
}

+ (BOOL)isPermittedScheme:(NSString *)urlScheme
{
	if ([urlScheme isEqualToString:@"http"] ||
		[urlScheme isEqualToString:@"https"])
	{
		return YES;
	}

	id permittedSchemesAny = [[NSUserDefaults standardUserDefaults] objectForKey:@"com.adiumX.AutoHyperlinks.permittedSchemesAny"];

	if (permittedSchemesAny && [permittedSchemesAny boolValue] == YES) {
		return YES;
	}

	NSArray *permittedSchemes = [[NSUserDefaults standardUserDefaults] arrayForKey:@"com.adiumX.AutoHyperlinks.permittedSchemes"];

	if (permittedSchemes && [permittedSchemes containsObject:urlScheme]) {
		return YES;
	}

	return NO;
}

- (NSArray *)nextURI
{
	NSRange	scannedRange;
	
	unsigned long scannedLocation = m_scanLocation;
	
	[self _scanString:m_scanString charactersFromSet:startSet intoRange:nil fromIndex:&scannedLocation];
	
	while ([self _scanString:m_scanString upToCharactersFromSet:skipSet intoRange:&scannedRange fromIndex:&scannedLocation])
	{
		if (MIN_LINK_LENGTH < scannedRange.length) {
			NSString *topEncChar = [m_openEnclosureStack lastObject];

			if (topEncChar || [enclosureSet characterIsMember:[m_scanString characterAtIndex:scannedRange.location]]) {
				unsigned long encIdx = 0;

				if (topEncChar) {
					encIdx = [enclosureStartArray indexOfObject:topEncChar];
				} else {
					encIdx = [enclosureStartArray indexOfObject:[m_scanString substringWithRange:NSMakeRange(scannedRange.location, 1)]];
				}

				NSRange encRange;

				if (encIdx != NSNotFound) {
					encRange = [m_scanString rangeOfString:[enclosureStopArray objectAtIndex:encIdx] options:NSBackwardsSearch range:scannedRange];

					if (encRange.location != NSNotFound) {
						scannedRange.length--;

						if (topEncChar) {
							[m_openEnclosureStack removeLastObject];
						} else {
							scannedRange.location++;
							scannedRange.length--;
						}
					} else {
						[m_openEnclosureStack addObject:[enclosureStartArray objectAtIndex:encIdx]];
					}
				}
			}

			if (scannedRange.length == 0) {
				break;
			}

			NSRange longestEnclosure = [self _longestBalancedEnclosureInRange:scannedRange];

			while (scannedRange.length > 2 && [endSet characterIsMember:[m_scanString characterAtIndex:(scannedRange.location + scannedRange.length - 1)]]) {
				if ((longestEnclosure.location + longestEnclosure.length) < scannedRange.length) {
					scannedRange.length--;
				} else {
					break;
				}
			}

			m_scanLocation = scannedRange.location;

			if (MIN_LINK_LENGTH < scannedRange.length) {
				NSString *_scanString = [m_scanString substringWithRange:scannedRange];

				AHParserStatus _parserStatus = [AHHyperlinkScanner isStringValidURI:_scanString usingStrict:m_strictChecking fromIndex:&m_scanLocation];

				if (_parserStatus != AHParserInvalidURLStatus) {
					if (_parserStatus == AHParserURLWithoutSchemeStatus && scannedRange.location >= 1) {
						unichar leftmost = [m_scanString characterAtIndex:(scannedRange.location - 1)];

						if (leftmost != '@' && leftmost != '.') {
							return [self _returnedValueWithProperURLScheme:_scanString inRange:scannedRange parserStatus:_parserStatus];
						}
					} else {
						return [self _returnedValueWithProperURLScheme:_scanString inRange:scannedRange parserStatus:_parserStatus];
					}
				}
			}

			NSRange startRange = [m_scanString rangeOfCharacterFromSet:puncSet options:NSLiteralSearch range:scannedRange];

			if (startRange.location != NSNotFound) {
				m_scanLocation = (startRange.location + startRange.length);
			} else {
				m_scanLocation += scannedRange.length;
			}

			scannedLocation = m_scanLocation;
		}
	}
	
	m_scanLocation = m_scanStringLength;
	
	return nil;
}

#pragma mark -
#pragma mark Private Methods

+ (NSString *)_URLWithProperScheme:(NSString *)url parserStatus:(AHParserStatus)parserStatus
{
	NSString *properURL = url;

	if (parserStatus == AHParserURLWithoutSchemeStatus) {
		properURL = [DEFAULT_URL_SCHEME stringByAppendingString:url];
	} else if (parserStatus == AHParserURLIsSpecialCaseStatus_Reddit) {
		properURL = [@"https://www.reddit.com" stringByAppendingString:url];
	}

	return properURL;
}

- (NSArray *)_returnedValueWithProperURLScheme:(NSString *)scannedString inRange:(NSRange)scannedRange parserStatus:(AHParserStatus)parserStatus
{
	NSString *properURL = [AHHyperlinkScanner _URLWithProperScheme:scannedString parserStatus:parserStatus];

	return @[NSStringFromRange(scannedRange), properURL];
}

- (NSArray *)_allMatches
{
    NSMutableArray *rangeArray = [NSMutableArray array];
	
	m_scanLocation = 0; 
    
	while (m_scanLocation < [m_scanString length]) {
		NSArray *markedLinkData = [self nextURI];
		
		if (markedLinkData) {
			[rangeArray addObject:markedLinkData];
		}	
	}
	
	return rangeArray;
}

- (NSRange)_longestBalancedEnclosureInRange:(NSRange)inRange
{
	NSDictionary *encDict = nil;
	
	NSMutableArray *enclosureStack = nil;
	NSMutableArray *enclosureArray = nil;
	
	NSString *matchChar = nil;
	
	unsigned long encScanLocation = inRange.location;
	
	while (encScanLocation < (inRange.length + inRange.location))
	{
		[self _scanString:m_scanString upToCharactersFromSet:enclosureSet intoRange:nil fromIndex:&encScanLocation];
		
		if (encScanLocation >= (inRange.location + inRange.length)) {
			break;
		}
		
		matchChar = [m_scanString substringWithRange:NSMakeRange(encScanLocation, 1)];
		
		if ([enclosureStartArray containsObject:matchChar])
		{
			encDict = [NSDictionary	dictionaryWithObjects:@[@(encScanLocation), matchChar]
												  forKeys:encKeys];
			
			if (enclosureStack == nil) {
				enclosureStack = [NSMutableArray array];
			}
			
			[enclosureStack addObject:encDict];
		}
		else if ([enclosureStopArray containsObject:matchChar])
		{
			NSEnumerator *encEnumerator = [enclosureStack objectEnumerator];
			
			while ((encDict = [encEnumerator nextObject]))
			{
				unsigned long encTagIndex	 = [encDict[ENC_INDEX_KEY] unsignedLongValue];
				unsigned long encStartIndex  = [enclosureStartArray indexOfObjectIdenticalTo:encDict[ENC_CHAR_KEY]];
				
				if ([enclosureStopArray indexOfObjectIdenticalTo:matchChar] == encStartIndex)
				{
					NSRange encRange = NSMakeRange(encTagIndex, (encScanLocation - encTagIndex + 1));
					
					if (enclosureStack == nil) {
						enclosureStack = [NSMutableArray array];
					}
					
					if (enclosureArray == nil) {
						enclosureArray = [NSMutableArray array];
					}
					
					[enclosureStack removeObject:encDict];

					[enclosureArray addObject:NSStringFromRange(encRange)];
					
					break;
				}
			}
		}
		
		if (encScanLocation < (inRange.length + inRange.location)) {
			encScanLocation++;
		}
	}
	
	if (enclosureArray && [enclosureArray count]) {
		return NSRangeFromString([enclosureArray lastObject]);
	} else {
		return NSMakeRange(0, 0);
	}
}

- (BOOL)_scanString:(NSString *)inString upToCharactersFromSet:(NSCharacterSet *)inCharSet intoRange:(NSRange *)outRangeRef fromIndex:(unsigned long *)idx
{
	unichar			_curChar;

	NSRange			_outRange;

	unsigned long	_scanLength = [inString length];
	unsigned long	_idx;
	
	if (_scanLength <= *idx) {
		return NO;
	}
	
	for (_idx = *idx; _scanLength > _idx; _idx++) {
		_curChar = [inString characterAtIndex:_idx];
		
		if ([skipSet characterIsMember:_curChar] == NO) {
			break;
		}
	}
	
	for (*idx = _idx; _scanLength > _idx; _idx++) {
		_curChar = [inString characterAtIndex:_idx];
		
		if ([inCharSet characterIsMember:_curChar] || 
			[skipSet characterIsMember:_curChar]) {
			
			break;
		}
	}
	
	_outRange = NSMakeRange(*idx, (_idx - *idx));
	
	*idx = _idx;
	
	if (_outRange.length) {
		if (outRangeRef) {
			*outRangeRef = _outRange;
		}
		
		return YES;
	} else {
		return NO;
	}
}

- (BOOL)_scanString:(NSString *)inString charactersFromSet:(NSCharacterSet *)inCharSet intoRange:(NSRange *)outRangeRef fromIndex:(unsigned long *)idx
{
	unichar			_curChar;

	NSRange			_outRange;

	unsigned long	_scanLength = [inString length];
	unsigned long	_idx = *idx;
	
	if (_scanLength <= _idx) {
		return NO;
	}
	
	for (_idx = *idx; _scanLength > _idx; _idx++) {
		_curChar = [inString characterAtIndex:_idx];
		
		if ([skipSet characterIsMember:_curChar] == NO) {
			break;
		}
	}
	
	for (*idx = _idx; _scanLength > _idx; _idx++) {
		_curChar = [inString characterAtIndex:_idx];
		
		if ([inCharSet characterIsMember:_curChar] == NO) {
			break;
		}
	}
	
	_outRange = NSMakeRange(*idx, (_idx - *idx));
	
	*idx = _idx;
	
	if (_outRange.length) {
		if (outRangeRef) {
			*outRangeRef = _outRange;
		}
		
		return YES;
	} else {
		return NO;
	}
}

@end
