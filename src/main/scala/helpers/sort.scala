package helpers

import helpers.GraphError.CyclicDependencyError

import scala.util.control.NoStackTrace

extension [K, V](items: Map[K, V])
    def topologicalSort(dependencies: V => Iterable[K]): Either[GraphError, List[(K, V)]] =
        @annotation.tailrec
        def loop(
                    remaining: Map[K, V],
                    sorted: List[(K, V)],
                    visited: Set[K],
                    recursionStack: Set[(K, V)]
                ): Either[GraphError, List[(K, V)]] =
            if remaining.isEmpty then Right(sorted)
            else
                val (noDeps, hasDeps) = remaining.partition: (_, value) =>
                    dependencies(value).forall(visited.contains)
                if noDeps.isEmpty then
                    if hasDeps.exists(recursionStack.contains) then Left(CyclicDependencyError)
                    else loop(hasDeps, sorted, visited, recursionStack ++ hasDeps)
                else loop(hasDeps, sorted ++ noDeps.toList, visited ++ noDeps.keySet, recursionStack)

        loop(items, List.empty, Set.empty, Set.empty)

enum GraphError extends RuntimeException, NoStackTrace:
    case CyclicDependencyError
    case MissingDependency[T](missing: Set[T])

end GraphError