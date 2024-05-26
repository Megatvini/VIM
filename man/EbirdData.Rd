\name{EbirdData}
\docType{data}
\alias{EbirdData}
\title{
  Training and Test sets for Multispecies Ebird data
}
\description{
  Training and Test sets for Multispecies Ebird data
}
\usage{
EbirdData
}
\format{
  List containing
  \itemize{
    \item{trainX,testX1}{ data frames containing covariates for 
		training (14037 by 87) and test (500 by 87) sets for the Ebird data.
	}
	\item{trainY,testY1}{ data frames containing responses for training(14037 by 6)
	and test (500 by 6) of counts observed for each of 5 species. 
    }
  }
  Note that column `X` is a row identifier that can be used to match responses
  to covariates in each case. 
}

\source{
  Sikdar, Sharmistha, Giles Hooker, and Vrinda Kadiyali. 
  "Variable importance measures for variable selection and 
  statistical inference in multivariate random forests." (2023).
}

% docclass is function
\keyword{datasets}

