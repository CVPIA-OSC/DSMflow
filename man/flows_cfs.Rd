% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{flows_cfs}
\alias{flows_cfs}
\title{Flow in cfs}
\format{
named list with two components, each containing a dataframe with 985 rows and 33 variables:
\describe{
\item{date}{CALSIM II date}
\item{Upper Sacramento River}{C104}
\item{Antelope Creek}{C11307}
\item{Battle Creek}{C10803}
\item{Bear Creek}{C11001*}
\item{Big Chico Creek}{C11501}
\item{Butte Creek}{C217A}
\item{Clear Creek}{C3}
\item{Cottonwood Creek}{C10802}
\item{Cow Creek}{C10801}
\item{Deer Creek}{C11309}
\item{Elder Creek}{C11303}
\item{Mill Creek}{C11308}
\item{Paynes Creek}{C11001}
\item{Stony Creek}{C142A}
\item{Thomes Creek}{C11304}
\item{Upper-mid Sacramento River}{C115}
\item{Bear River}{C285}
\item{Feather River}{C203}
\item{Yuba River}{C230}
\item{Lower-mid Sacramento River1}{C134}
\item{Lower-mid Sacramento River2}{C160}
\item{American River}{C9}
\item{Lower Sacramento River}{C166}
\item{Calaveras River}{C92}
\item{Cosumnes River}{C501}
\item{Mokelumne River}{C91**}
\item{Merced River}{C561}
\item{Stanislaus River}{C520}
\item{Tuolumne River}{C540}
\item{San Joaquin River}{C630}
}
}
\source{
\itemize{
\item \strong{Data Wrangling:} Sadie Gill  \email{sgill@flowwest.com}
\item \strong{Node Selection:} Mark Tompkins \email{mtompkins@flowwest.com} and Mike Urkov \email{mike.urkov@gmail.com}
\item \strong{CALSIM Model Output:} Michael Wright \email{mwright@usbr.gov}
}
}
\usage{
flows_cfs
}
\description{
A named list containing datasets from the 2008-2009 and 2018-2019 Calsim II BiOp runs for
use with the CVPIA SIT Salmon Population Model to develop habitat inputs.
The named list components are:
\itemize{
\item biop_2008_2009: flow in cubic feet per second within watersheds from the 2008-2009 Calsim II BiOp run.
\item biop_itp_2018_2019: flow in cubic feet per second within watersheds from the 2018-2019 Calsim II BiOp run.
}
}
\details{
The flow is represented using 'FLOW-CHANNEL' and 'FLOW-DELIVERY' nodes from CALSIM II.
The nodes and calculation for each watershed are outlined above.

The Sutter and Yolo Bypasses' flows are stored in a separate dataset
\code{\link{bypass_flows}} to enable the habitat segmentation in the bypasses
according to weir locations.

The Lower-mid Sacramento River has two nodes, one above Fremont Weir (C134) and one below (C160).
When calculating habitat for the Lower-Mid Sacramento river, calculate the habitat at each flow node and
sum them proportion to the length of stream above and below the weir:
\deqn{35.6/58 * (habitat at C134) + 22.4/58 * (habitat at C160)}

\emph{*Because there are no modeled flows at Bear Creek, flows at Paynes Creek are used}

\emph{**Mokelumne River flow is from a separate model provided by EBMUD.}

\href{https://s3-us-west-2.amazonaws.com/cvpiaflow-r-package/BST_CALSIMII_schematic_040110.jpg}{CALSIM II schematic}
}
\keyword{datasets}
