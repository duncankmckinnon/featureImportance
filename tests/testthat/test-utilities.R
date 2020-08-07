describe('utilities', {
  describe("remove_last_n()", {
    describe('single entry', {
      str <- paste0(c('a','b','c','d'), sep = '+', collapse = '')
      it("should remove the last n of a string", {
        str <- paste0(c('a','b','c','d'), sep = '+', collapse = '')
        teststr <- remove_last_n(str, 1)
        expect_equal(teststr, 'a+b+c+d')
      })
      it("should return full string when n is < 1", {
        str <- paste0(c('a','b','c','d'), sep = '+', collapse = '')
        teststr <- remove_last_n(str, -1)
        expect_equal(teststr, 'a+b+c+d+')
      })
      it("should return empty string when n is >= |str|", {
        str <- paste0(c('a','b','c','d'), sep = '+', collapse = '')
        teststr <- remove_last_n(str, 1000)
        expect_equal(teststr, '')
      })
      it("should replace missing string with missing", {
        str <- paste0(c('a','b','c','d'), sep = '+', collapse = '')
        teststr <- remove_last_n(str, 1000, '...')
        expect_equal(teststr, '...')
      })
    })
    describe('vectorized entry', {
      it('should remove the last n of each string', {
        strs <- c('a+b+','a^2^','a-')
        teststr <- remove_last_n(strs, 2)
        expect_equal(teststr, c('a+','a^',''))
      })
      it('should replace over-truncated strings with missing', {
        strs <- c('a+b+','a^2^','a-')
        teststr <- remove_last_n(strs, 4, '...')
        expect_equal(teststr, c('...','...','...'))
      })
    })
  })

  describe('format_formula()', {
    describe('additive features', {
      it('should successfully generate additive formula with single element', {
        expect_equal( format_formula( 'y', xadd = '-a' ), formula( 'y~-a' ) )
      })
      it('should successfully generate additive formula', {
        expect_equal( format_formula( 'y', xadd = c( 'a','b','c','d','e' ) ), formula( 'y~a+b+c+d+e' ) )
      })
      it('should throw an error when xadd is not formatted correctly', {
        expect_error(format_formula('y', xadd = c(1,2,3)), 'Error: xadd format')
      })
    })
    describe('multiplicative features', {
      it('should successfully generate single multiplicative formula with single element', {
        expect_equal( format_formula( 'y', xmultiply = 'a' ), formula( 'y~a' ) )
      })
      it('should successfully generate single multiplicative formula', {
        expect_equal( format_formula( 'y', xmultiply = c( 'a','b','c','d','e' ) ), formula( 'y~a*b*c*d*e' ) )
      })
      it('should successfully generate multiple multiplicative formula', {
        expect_equal( format_formula( 'y', xmultiply = list(c( 'a','b' ), c('c','d','e' ) ) ), formula( 'y~a*b+c*d*e' ) )
      })

      it('should successfully generate multiple multiplicative formula with addition', {
        expect_equal( format_formula( 'y', xadd = c( 'a','b','c' ), xmultiply = list( c( 'a','b' ),c( 'c','d','e' ) ) ), formula( 'y~a+b+c+a*b+c*d*e' ) )
      })
      it('should successfully generate single multiplicative formula with single element with addition', {
        expect_equal( format_formula( 'y', xadd = c( 'a','b','c'), xmultiply = 'a' ), formula( 'y~a+b+c+a' ) )
      })
      it('should throw an error when xmultiply is not formatted correctly as vector', {
        expect_error( format_formula( 'y', xmultiply = 1:3 ), 'Error: xmultiply format' )
      })
      it('should throw an error when xmultiply is not formatted correctly as list', {
        expect_error( format_formula( 'y', xadd = c('a','b'), xmultiply = list( 1:3, c( 'a','b' ) ) ), 'Error: xmultiply format' )
      })
    })
    describe('exponential features', {
      it('should successfully generate single exponential formula with single element',{
        expect_equal( format_formula( 'y', xpower = 'a' ), formula( 'y~a^2' ) )
      })
      it('should successfully generate single exponential formula with multiple elements',{
        expect_equal( format_formula( 'y', xpower = c('a','b','c') ), formula( 'y~a^2+b^2+c^2' ) )
      })
      it('should successfully generate single exponential formula with multiple elements and different power specified',{
        expect_equal( format_formula( 'y', xpower = c('a','b','c'), power = exp(1) ), formula( 'y~a^2.71828182845905+b^2.71828182845905+c^2.71828182845905' ) )
      })
      it('should successfully generate multiple exponential formula', {
        expect_equal( format_formula( 'y', xpower = list( c( 'a', 2 ), c('b', 1/2 ) ) ), formula( 'y~a^2+b^0.5') )
      })
      it('should successfully generate single exponential formula with multiplication', {
        expect_equal( format_formula( 'y', xmultiply = list( 'a', c( 'a', 'b' ) ), xpower = c( '-a', 'b' ), power = 3 ), formula( 'y~a+a*b-a^3+b^3') )
      })
      it('should successfully generate single exponential formula with multiplication and addition', {
        expect_equal( format_formula( 'y', xadd = c( 'a','b','c' ), xmultiply = list( c( 'a', 'd' ), c( 'c', 'b' ) ), xpower = c( 'a', 'b' ), power = 3 ), formula( 'y~a+b+c+a*d+c*b+a^3+b^3') )
      })
      it('should successfully generate multiple exponential formula with multiplication', {
        expect_equal( format_formula( 'y', xmultiply = list( 'a', c( '-a', 'b' ) ), xpower = list( c( 'a', 'b' ), c('b', 1/2 ) ) ), formula( 'y~a-a*b+a^b+b^0.5') )
      })
      it('should successfully generate multiple exponential formula with multiplication and addition', {
        expect_equal( format_formula( 'y', xadd = c( 'a','-b','c' ), xmultiply = list( c( 'a', 'd' ), c( '-c', 'b' ) ), xpower = list( c( 'a', 2 ), c('b', 1/2 ) ) ), formula( 'y~a-b+c+a*d-c*b+a^2+b^0.5') )
      })
      it('should throw an error when xpower is not formatted correctly as vector', {
        expect_error( format_formula( 'y',  xadd = c( 'a','b','c' ), xmultiply = c( 'a','b' ), xpower = 1:3, power = 2 ), 'Error: xpower format' )
      })
      it('should throw an error when xpower is not formatted correctly as list', {
        expect_error( format_formula( 'y', xadd = c( 'a','b' ), xmultiply = list( c( 'a','c' ), c( 'a','b' ) ), xpower = list( c('a','c' ), c(1, 3) ) ), 'Error: xpower format' )
      })
    })
  })
})

