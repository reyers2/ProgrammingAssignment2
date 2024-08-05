


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  # m 변수를 초기화
    v <- NULL  # v 변수를 초기화
    
    set <- function(y) {
        if (is.vector(y)) {
            # 벡터를 정사각 행렬로 변환
            size <- sqrt(length(y))
            if (size %% 1 != 0) {
                stop("The length of the vector must be a perfect square to form a square matrix")
            }
            y <- matrix(y, nrow = size, ncol = size)  # 벡터를 행렬로 변환
        }
        m <<- y
        v <<- NULL
    }
    
    get <- function() m
    
    setInverse <- function(inverse) v <<- inverse
    
    getInverse <- function() v
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    v <- x$getInverse()
    if (!is.null(v)) {
        message("getting cached inverse matrix")
        return(v)
    }
    m <- x$get()
    if (is.null(m)) {
        stop("Matrix is NULL. Please provide a valid matrix.")
    }
    if (nrow(m) != ncol(m)) {
        stop("Matrix must be square to compute its inverse.")
    }
    v <- solve(m, ...)
    x$setInverse(v)
    v
}

# 입력 데이터를 행렬로 설정
vector_data <- c(74, 24, 43, 33, 32, 38, 52, 64, 17)

# 캐시 행렬을 초기화
df <- makeCacheMatrix()
df$set(vector_data)  # set 함수를 명시적으로 호출하여 데이터를 설정

# 역행렬 계산, 캐시에 저장
inverse_matrix <- cacheSolve(df)

# 역행렬 출력
print(inverse_matrix)

