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

#import "AHLinkLexer.h"

#import "AHHyperlinkScanner.h"
#import "AHHyperlinkScannerResultPrivate.h"

NS_ASSUME_NONNULL_BEGIN

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

@interface AHHyperlinkScanner ()
@property (nonatomic, copy) NSString *scanString;
@property (nonatomic, assign) BOOL strictChecking;
@property (nonatomic, assign) NSUInteger scanLocation;
@property (nonatomic, assign) NSUInteger scanStringLength;
@property (nonatomic, strong) NSMutableArray *openEnclosureStack;
@end

@implementation AHHyperlinkScanner

static NSArray *s_enclosureKeys = nil;
static NSArray *s_enclosureStartArray	= nil;
static NSArray *s_enclosureStopArray = nil;

static NSCharacterSet *s_enclosureCharacterSet = nil;
static NSCharacterSet *s_endCharacterSet = nil;
static NSCharacterSet *s_hostnameComponentCharacterSet = nil;
static NSCharacterSet *s_punctuationCharacterSet = nil;
static NSCharacterSet *s_skipCharacterSet	= nil;
static NSCharacterSet *s_startCharacterSet = nil;

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
		
		s_skipCharacterSet = [NSCharacterSet characterSetWithBitmapRepresentation:mutableSkipSet.bitmapRepresentation];
		s_punctuationCharacterSet = [NSCharacterSet characterSetWithBitmapRepresentation:mutablePuncSet.bitmapRepresentation];
		s_startCharacterSet = [NSCharacterSet characterSetWithBitmapRepresentation:mutableStartSet.bitmapRepresentation];
		s_endCharacterSet = [NSCharacterSet characterSetWithCharactersInString:@"\"'“”‘’,:;>)]}–—.…?!@"];

		s_hostnameComponentCharacterSet = [NSCharacterSet characterSetWithCharactersInString:@"./"];

		s_enclosureStartArray = @[@"(",@"[",@"{"];
		s_enclosureStopArray = @[@")",@"]",@"}"];
		s_enclosureCharacterSet = [NSCharacterSet characterSetWithCharactersInString:@"()[]{}"];

		s_enclosureKeys = @[ENC_INDEX_KEY, ENC_CHAR_KEY];
	}
}

+ (AHHyperlinkScanner *)linkScanner
{
	return [[self class] new];
}

- (NSArray<AHHyperlinkScannerResult *> *)matchesForString:(NSString *)inString
{
	NSParameterAssert(inString != nil);

	self.strictChecking = NO;

	self.scanString = inString;
	self.scanStringLength = inString.length;

	self.openEnclosureStack = [NSMutableArray array];

	return [self _allMatches];
}

- (NSArray<AHHyperlinkScannerResult *> *)strictMatchesForString:(NSString *)inString
{
	NSParameterAssert(inString != nil);

	self.strictChecking = YES;

	self.scanString = inString;
	self.scanStringLength = inString.length;

	self.openEnclosureStack = [NSMutableArray array];

	return [self _allMatches];
}

+ (nullable NSString *)URLWithProperScheme:(NSString *)url
{
	AHParserStatus parserStatus = [self isStringValidURI:url usingStrict:NO fromIndex:NULL];

	if (parserStatus == AHParserInvalidURLStatus) {
		return nil;
	}

	return [self _URLWithProperScheme:url parserStatus:parserStatus];
}

+ (AHParserStatus)isStringValidURI:(NSString *)inString usingStrict:(BOOL)useStrictChecking fromIndex:(NSUInteger *)index
{
	const char *inStringEnc = [inString cStringUsingEncoding:NSUTF8StringEncoding];

	if (inStringEnc == NULL) {
		return AHParserInvalidURLStatus;
	}
	
	unsigned long inStringEncLength = strlen(inStringEnc);

	yyscan_t scanner;

	AHlex_init(&scanner);
	
    AH_BUFFER_STATE buffer = AH_scan_string(inStringEnc, scanner);
	
    AHParserStatus validStatus = (AHParserStatus)AHlex(scanner);

	if (validStatus == AHParserURLWithWildcardSchemeStatus) {
		NSRange schemeColonRange = [inString rangeOfString:@":"];

		if (schemeColonRange.location != NSNotFound) {
			NSString *urlScheme = [inString substringToIndex:schemeColonRange.location];

			if ([self isPermittedScheme:urlScheme] == NO) {
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
			if ( index) {
				*index += inString.length;
			}

			AHlex_destroy(scanner);
			
            return validStatus;
        }
    } else {
        AH_delete_buffer(buffer, scanner);
		
        buffer = NULL;
	}

	if ( index) {
		*index += AHget_leng(scanner);
	}

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

	/* Two seperate arrays exist so an app can specify its own defaults in
	 NSUserDefaults while allowing a user to whitelist additional schemes,
	 without having to respecify the app's original. */
	NSArray<NSString *> *permittedSchemesDefaults = [[NSUserDefaults standardUserDefaults] arrayForKey:@"com.adiumX.AutoHyperlinks.permittedSchemesDefault"];

	if (permittedSchemesDefaults && [permittedSchemesDefaults containsObject:urlScheme]) {
		return YES;
	}

	NSArray<NSString *> *permittedSchemes = [[NSUserDefaults standardUserDefaults] arrayForKey:@"com.adiumX.AutoHyperlinks.permittedSchemes"];

	if (permittedSchemes && [permittedSchemes containsObject:urlScheme]) {
		return YES;
	}

	return NO;
}

- (nullable AHHyperlinkScannerResult *)nextURI
{
	NSRange	scannedRange;
	
	NSUInteger scannedLocation = self->_scanLocation;
	
	[self _scanString:self->_scanString charactersFromSet:s_startCharacterSet intoRange:nil fromIndex:&scannedLocation];
	
	while ([self _scanString:self->_scanString upToCharactersFromSet:s_skipCharacterSet intoRange:&scannedRange fromIndex:&scannedLocation])
	{
		if (MIN_LINK_LENGTH < scannedRange.length) {
			NSString *topEncChar = self->_openEnclosureStack.lastObject;

			if (topEncChar || [s_enclosureCharacterSet characterIsMember:[self->_scanString characterAtIndex:scannedRange.location]]) {
				NSUInteger encIndex = 0;

				if (topEncChar) {
					encIndex = [s_enclosureStartArray indexOfObject:topEncChar];
				} else {
					encIndex = [s_enclosureStartArray indexOfObject:[self->_scanString substringWithRange:NSMakeRange(scannedRange.location, 1)]];
				}

				NSRange encRange;

				if (encIndex != NSNotFound) {
					encRange = [self->_scanString rangeOfString:s_enclosureStopArray[encIndex] options:NSBackwardsSearch range:scannedRange];

					if (encRange.location != NSNotFound) {
						scannedRange.length--;

						if (topEncChar) {
							[self->_openEnclosureStack removeLastObject];
						} else {
							scannedRange.location++;
							scannedRange.length--;
						}
					} else {
						[self->_openEnclosureStack addObject:s_enclosureStartArray[encIndex]];
					}
				}
			}

			if (scannedRange.length == 0) {
				break;
			}

			NSRange longestEnclosure = [self _longestBalancedEnclosureInRange:scannedRange];

			while (scannedRange.length > 2 && [s_endCharacterSet characterIsMember:[self->_scanString characterAtIndex:(scannedRange.location + scannedRange.length - 1)]]) {
				if ((longestEnclosure.location + longestEnclosure.length) < scannedRange.length) {
					scannedRange.length--;
				} else {
					break;
				}
			}

			self->_scanLocation = scannedRange.location;

			if (MIN_LINK_LENGTH < scannedRange.length) {
				NSString *scanString = [self->_scanString substringWithRange:scannedRange];

				AHParserStatus _parserStatus = [[self class] isStringValidURI:scanString usingStrict:self->_strictChecking fromIndex:&self->_scanLocation];

				if (_parserStatus != AHParserInvalidURLStatus) {
					if (_parserStatus == AHParserURLWithoutSchemeStatus && scannedRange.location >= 1) {
						UniChar leftmost = [self->_scanString characterAtIndex:(scannedRange.location - 1)];

						if (leftmost != '@' && leftmost != '.') {
							return [self _returnedValueWithProperURLScheme:scanString inRange:scannedRange parserStatus:_parserStatus];
						}
					} else {
						return [self _returnedValueWithProperURLScheme:scanString inRange:scannedRange parserStatus:_parserStatus];
					}
				}
			}

			NSRange startRange = [self->_scanString rangeOfCharacterFromSet:s_punctuationCharacterSet options:NSLiteralSearch range:scannedRange];

			if (startRange.location != NSNotFound) {
				self->_scanLocation = (startRange.location + startRange.length);
			} else {
				self->_scanLocation += scannedRange.length;
			}

			scannedLocation = self->_scanLocation;
		}
	}
	
	self->_scanLocation = self->_scanStringLength;
	
	return nil;
}

#pragma mark -
#pragma mark Private Methods

+ (NSString *)_URLWithProperScheme:(NSString *)url parserStatus:(AHParserStatus)parserStatus
{
	NSString *urlProper = url;

	if (parserStatus == AHParserURLWithoutSchemeStatus) {
		urlProper = [DEFAULT_URL_SCHEME stringByAppendingString:url];
	} else if (parserStatus == AHParserURLIsSpecialCaseStatus_Reddit) {
		urlProper = [@"https://www.reddit.com" stringByAppendingString:url];
	}

	urlProper =
	[urlProper stringByReplacingOccurrencesOfString:@"\"" withString:@"%22"];

	return urlProper;
}

- (AHHyperlinkScannerResult *)_returnedValueWithProperURLScheme:(NSString *)url inRange:(NSRange)range parserStatus:(AHParserStatus)parserStatus
{
	NSString *urlProper = [[self class] _URLWithProperScheme:url parserStatus:parserStatus];

	  AHHyperlinkScannerResult *result =
	[[AHHyperlinkScannerResult alloc] initWithString:urlProper
											 inRange:range
										 strictMatch:(parserStatus == AHParserURLWithRecognizedSchemeStatus ||
													  parserStatus == AHParserURLWithWildcardSchemeStatus)];

	return result;
}

- (NSArray<AHHyperlinkScannerResult *> *)_allMatches
{
    NSMutableArray<AHHyperlinkScannerResult *> *resultArray = [NSMutableArray array];
	
	self->_scanLocation = 0;
    
	while (self->_scanLocation < self->_scanStringLength) {
		AHHyperlinkScannerResult *result = [self nextURI];
		
		if ( result) {
			[resultArray addObject:result];
		}	
	}
	
	return [resultArray copy];
}

- (NSRange)_longestBalancedEnclosureInRange:(NSRange)inRange
{
	NSDictionary *encDict = nil;

	NSMutableArray *enclosureArray = nil;
	NSMutableArray *enclosureStack = nil;
	
	NSString *matchCharacter = nil;
	
	NSUInteger encScanLocation = inRange.location;
	
	while (encScanLocation < (inRange.length + inRange.location))
	{
		[self _scanString:self->_scanString upToCharactersFromSet:s_enclosureCharacterSet intoRange:nil fromIndex:&encScanLocation];
		
		if (encScanLocation >= (inRange.location + inRange.length)) {
			break;
		}
		
		matchCharacter = [self->_scanString substringWithRange:NSMakeRange(encScanLocation, 1)];
		
		if ([s_enclosureStartArray containsObject:matchCharacter])
		{
			encDict = [NSDictionary	dictionaryWithObjects:@[@(encScanLocation), matchCharacter]
												  forKeys:s_enclosureKeys];
			
			if (enclosureStack == nil) {
				enclosureStack = [NSMutableArray array];
			}
			
			[enclosureStack addObject:encDict];
		}
		else if ([s_enclosureStopArray containsObject:matchCharacter])
		{
			NSEnumerator *encEnumerator = [enclosureStack objectEnumerator];
			
			while ((encDict = [encEnumerator nextObject]))
			{
				NSUInteger encTagIndex = [encDict[ENC_INDEX_KEY] unsignedIntegerValue];

				NSUInteger encStartIndex = [s_enclosureStartArray indexOfObjectIdenticalTo:encDict[ENC_CHAR_KEY]];
				
				if ([s_enclosureStopArray indexOfObjectIdenticalTo:matchCharacter] == encStartIndex)
				{
					NSRange encRange = NSMakeRange(encTagIndex, (encScanLocation - encTagIndex + 1));

					if (enclosureArray == nil) {
						enclosureArray = [NSMutableArray array];
					}

					if (enclosureStack == nil) {
						enclosureStack = [NSMutableArray array];
					}
					
					[enclosureStack removeObject:encDict];

					[enclosureArray addObject:[NSValue valueWithRange:encRange]];
					
					break;
				}
			}
		}
		
		if (encScanLocation < (inRange.length + inRange.location)) {
			encScanLocation++;
		}
	}
	
	if (enclosureArray && enclosureArray.count > 0) {
		id lastObject = enclosureArray.lastObject;

		return [lastObject rangeValue];
	}

	return NSMakeRange(0, 0);
}

- (BOOL)_scanString:(NSString *)inString upToCharactersFromSet:(NSCharacterSet *)inCharacterSet intoRange:(NSRange *)outRangeRef fromIndex:(NSUInteger *)index
{
	NSUInteger _scanLength = inString.length;

	if (_scanLength <= *index) {
		return NO;
	}

	NSRange	_outRange;

	NSUInteger _index;
	
	for (_index = *index; _scanLength > _index; _index++) {
		UniChar _currentCharacter = [inString characterAtIndex:_index];
		
		if ([s_skipCharacterSet characterIsMember:_currentCharacter] == NO) {
			break;
		}
	}
	
	for (*index = _index; _scanLength > _index; _index++) {
		UniChar _currentCharacter = [inString characterAtIndex:_index];
		
		if ([s_skipCharacterSet characterIsMember:_currentCharacter] ||
			[inCharacterSet characterIsMember:_currentCharacter])
		{
			break;
		}
	}
	
	_outRange = NSMakeRange(*index, (_index - *index));
	
	*index = _index;
	
	if (_outRange.length > 0) {
		if ( outRangeRef) {
			*outRangeRef = _outRange;
		}
		
		return YES;
	}

	return NO;
}

- (BOOL)_scanString:(NSString *)inString charactersFromSet:(NSCharacterSet *)inCharacterSet intoRange:(NSRange *)outRangeRef fromIndex:(NSUInteger *)index
{
	NSUInteger _scanLength = inString.length;

	if (_scanLength <= *index) {
		return NO;
	}

	NSRange	_outRange;

	NSUInteger _index;
	
	for (_index = *index; _scanLength > _index; _index++) {
		UniChar _currentCharacter = [inString characterAtIndex:_index];
		
		if ([s_skipCharacterSet characterIsMember:_currentCharacter] == NO) {
			break;
		}
	}
	
	for (*index = _index; _scanLength > _index; _index++) {
		UniChar _currentCharacter = [inString characterAtIndex:_index];
		
		if ([inCharacterSet characterIsMember:_currentCharacter] == NO) {
			break;
		}
	}
	
	_outRange = NSMakeRange(*index, (_index - *index));
	
	*index = _index;
	
	if (_outRange.length > 0) {
		if ( outRangeRef) {
			*outRangeRef = _outRange;
		}
		
		return YES;
	}

	return NO;
}

@end

NS_ASSUME_NONNULL_END
